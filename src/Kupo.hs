--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Kupo
    ( -- * Commands
      runWith
    , version
    , healthCheck
    , copyDatabase

    -- * Kupo
    , Kupo (..)
    , kupo
    , kupoWith

    -- * Environment
    , Env (..)
    , newEnvironment
    , newEnvironmentWith

    -- * Command & Options
    , Command (..)
    , parseOptions

    -- * Tracers
    , Tracers(..)
    , TracersCopy(..)
    , Severity(..)
    , withTracers
    , defaultTracers
    ) where

import Kupo.Prelude

import Control.Exception.Safe
    ( isAsyncException
    )
import Data.Pool
    ( defaultPoolConfig
    , destroyAllResources
    , newPool
    , tryWithResource
    , withResource
    )
import GHC.Conc
    ( getNumCapabilities
    )
import Kupo.App
    ( ChainSyncClient
    , TraceConsumer (..)
    , TraceKupo (..)
    , consumer
    , gardener
    , idle
    , mkNotifyTip
    , newProducer
    , readOnlyConsumer
    , withFetchBlockClient
    )
import Kupo.App.ChainSync
    ( withChainSyncExceptionHandler
    )
import Kupo.App.Configuration
    ( TraceConfiguration (..)
    , newPatternsCache
    , startOrResume
    )
import Kupo.App.Database
    ( ConnectionType (..)
    , Database (..)
    , copyDatabase
    , createShortLivedConnection
    , newDatabaseFile
    , newLock
    , withLongLivedConnection
    , withShortLivedConnection
    )
import Kupo.App.Health
    ( connectionStatusToggle
    , initializeHealth
    , readHealth
    )
import Kupo.App.Http
    ( healthCheck
    , httpServer
    )
import Kupo.App.Mailbox
    ( Mailbox
    )
import Kupo.Control.MonadAsync
    ( concurrently4
    )
import Kupo.Control.MonadCatch
    ( handle
    )
import Kupo.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    , logWith
    , traceWith
    , withTracers
    )
import Kupo.Control.MonadSTM
    ( MonadSTM (..)
    )
import Kupo.Control.MonadThrow
    ( finally
    , throwIO
    )
import Kupo.Data.Cardano
    ( IsBlock
    , Point
    , Tip
    )
import Kupo.Data.ChainSync
    ( ForcedRollbackHandler
    )
import Kupo.Data.Configuration
    ( Configuration (..)
    , DeferIndexesInstallation (..)
    , NodeTipHasBeenReached (..)
    , OperationalMode (..)
    )
import Kupo.Data.FetchBlock
    ( FetchBlockClient
    )
import Kupo.Data.Health
    ( Health
    , emptyHealth
    )
import Kupo.Options
    ( Command (..)
    , Tracers (..)
    , TracersCopy (..)
    , parseOptions
    )
import Kupo.Version
    ( version
    )
import System.Exit
    ( ExitCode (..)
    )

--
-- Environment
--

-- | Main application monad.
newtype Kupo a = Kupo
    { unKupo :: ReaderT (Env Kupo) IO a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Env Kupo)
        , MonadIO
        )

-- | Application entry point.
kupo :: Tracers IO Concrete -> Kupo ()
kupo tr = do
    Env { configuration = Configuration
            { chainProducer
            }
        } <- ask
    kupoWith tr
        (newProducer (tracerConfiguration tr) chainProducer)
        (withFetchBlockClient chainProducer)

-- | Same as 'kupo', but allows specifying the chain producer component.
kupoWith
    :: Tracers IO Concrete
    -> ( ( forall block. IsBlock block
          => (Point -> ForcedRollbackHandler IO -> IO ())
          -> Mailbox IO (Tip, block) (Tip, Point)
          -> ChainSyncClient IO block
          -> IO ()
         )
         -> IO ()
       )
       -- ^ Chain producer acquisition bracket
    -> ( ( forall block. IsBlock block
          => FetchBlockClient IO block
          -> IO ()
         )
         -> IO ()
       )
       -- ^ FetchBlockClient acquisition bracket
    -> Kupo ()
kupoWith tr withProducer withFetchBlock =
  hijackSigTerm *> do
    Env { health
        , crashWith
        , configuration = config@Configuration
            { serverHost
            , serverPort
            , workDir
            , inputManagement
            , longestRollback
            , deferIndexes
            , operationalMode
            }
        } <- ask

    (maxConcurrentWriters, maxConcurrentReaders) <- liftIO getNumCapabilities <&> \n -> (n,  5 * n)

    liftIO $ logWith (tracerConfiguration tr) $
        ConfigurationMaxConcurrency { maxConcurrentReaders, maxConcurrentWriters }

    dbFile <- newDatabaseFile (tracerDatabase tr) workDir

    lock <- liftIO newLock

    readOnlyPool <- liftIO $ newPool $ defaultPoolConfig
        (createShortLivedConnection (tracerDatabase tr) ReadOnly lock longestRollback dbFile)
        (\Database{close} -> close)
        600
        maxConcurrentReaders

    readWritePool <- liftIO $ newPool $ defaultPoolConfig
        (createShortLivedConnection (tracerDatabase tr) ReadWrite lock longestRollback dbFile)
        (\Database{close} -> close)
        30
        maxConcurrentWriters

    let run action =
            case operationalMode of
                FullServer ->
                    handle
                        (\NodeTipHasBeenReached{distance} -> do
                            traceWith (tracerKupo tr) KupoRestartingWithIndexes { distance }
                            io InstallIndexesIfNotExist
                        )
                        (io deferIndexes)
                  where
                    io indexMode =
                        withLongLivedConnection
                            (tracerDatabase tr)
                            lock
                            longestRollback
                            dbFile
                            indexMode
                            (action indexMode)
                          `finally` do
                               destroyAllResources readOnlyPool
                               destroyAllResources readWritePool
                ReadOnlyReplica ->
                    -- NOTE: 'ShortLived' is a bad name here. What it really means is 'occasional
                    -- writers but mostly readers'. However, in the 'ReadOnlyReplica' mode we only
                    -- ever allow read-only connections and never perform a single write.
                    withShortLivedConnection
                        (tracerDatabase tr)
                        ReadOnly
                        lock
                        longestRollback
                        dbFile
                        (action indexMode)

    liftIO $ handle (onUnknownException crashWith) $ run $ \indexMode db -> do
        patterns <- newPatternsCache (tracerConfiguration tr) config db
        let statusToggle = connectionStatusToggle health
        let tracerChainSync =  contramap ConsumerChainSync . tracerConsumer
        withProducer $ \forceRollback mailbox producer -> do
            withFetchBlock $ \fetchBlock -> do
                concurrently4
                    -- HTTP Server
                    ( httpServer
                        (tracerHttp tr)
                        (\case
                            ReadOnly  ->
                                tryWithResource readOnlyPool
                            ReadWrite ->
                                case operationalMode of
                                    FullServer ->
                                        tryWithResource readWritePool
                                    ReadOnlyReplica ->
                                        const (fail "cannot acquire read/write connection on read-only replica.")
                            WriteOnly ->
                                const (fail "impossible: tried to acquire WriteOnly database?")
                        )
                        forceRollback
                        fetchBlock
                        patterns
                        (readHealth health)
                        serverHost
                        serverPort
                    )

                    -- Block consumer fueling the database
                    ( case operationalMode of
                        FullServer ->
                            consumer
                                (tracerConsumer tr)
                                inputManagement
                                (mkNotifyTip indexMode health)
                                mailbox
                                patterns
                                db
                        ReadOnlyReplica ->
                            readOnlyConsumer
                                health
                                db
                    )

                    -- Database garbage-collector
                    ( case operationalMode of
                        FullServer ->
                            gardener
                                (tracerGardener tr)
                                config
                                patterns
                                (withResource readWritePool)
                        ReadOnlyReplica ->
                            idle
                    )

                    -- Block producer, fetching blocks from the network
                    ( case operationalMode of
                        FullServer ->
                            withChainSyncExceptionHandler (tracerChainSync tr) statusToggle $ do
                                (mostRecentCheckpoint, checkpoints) <- startOrResume (tracerConfiguration tr) config db
                                initializeHealth health mostRecentCheckpoint
                                producer
                                    (tracerChainSync tr)
                                    checkpoints
                                    statusToggle
                        ReadOnlyReplica ->
                            idle
                    )

  where
    onUnknownException :: (SomeException -> IO ()) -> SomeException -> IO ()
    onUnknownException crashWith e
        | isAsyncException e = do
            throwIO e
        | otherwise = do
            logWith (tracerKupo tr) $ KupoUnexpectedError (toText (displayException e))
            crashWith e

--
-- Environment
--

-- | Application runner with an instantiated environment. See 'newEnvironment'.
runWith :: forall a. Kupo a -> Env Kupo -> IO a
runWith app = runReaderT (unKupo app)

data Env (m :: Type -> Type) = Env
    { crashWith :: SomeException -> IO ()
    , configuration :: !Configuration
    , health :: !(TVar IO Health)
    } deriving stock (Generic)

newEnvironment
    :: Configuration
    -> IO (Env Kupo)
newEnvironment =
    newEnvironmentWith (const (exitWith (ExitFailure 1)))

newEnvironmentWith
    :: (SomeException -> IO ())
    -> Configuration
    -> IO (Env Kupo)
newEnvironmentWith crashWith configuration = do
    health <- newTVarIO emptyHealth
    pure Env{configuration, health, crashWith}
