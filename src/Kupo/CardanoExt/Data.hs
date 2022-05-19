{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Kupo.CardanoExt.Data
    ( BinaryData(..)
    , binaryDataToData
    , dataToBinaryData
    )
where
import Kupo.Prelude 
import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    decodeNestedCborBytes,
    encodeTag,
  )
import Data.MemoBytes (MemoBytes (..))


import Cardano.Ledger.Alonzo.Data (Data(DataConstr))

-- Taken from cardano-ledger. 
-- I failed to bump the dependencies, so instead just extracted the relevant lines. 
-- TODO: remove when dependencies are bumped

newtype BinaryData era = BinaryData ShortByteString
  deriving newtype (Eq, Ord, Show)

instance Typeable era => ToCBOR (BinaryData era) where
  toCBOR (BinaryData sbs) = encodeTag 24 <> toCBOR sbs

instance Typeable era => FromCBOR (BinaryData era) where
  fromCBOR = do
    bs <- decodeNestedCborBytes
    either fail pure $! makeBinaryData (toShort bs)

makeBinaryData :: ShortByteString -> Either String (BinaryData era)
makeBinaryData sbs = do
  let binaryData = BinaryData sbs
  -- We need to verify that binary data is indeed valid Plutus Data.
  case decodeBinaryData binaryData of
    Left e -> Left $ "Invalid CBOR for Data: " <> show e
    Right _d -> Right binaryData

decodeBinaryData :: BinaryData era -> Either DecoderError (Data era)
decodeBinaryData (BinaryData sbs) = do
  plutusData <- decodeAnnotator "Data" fromCBOR (fromStrict (fromShort sbs))
  pure (DataConstr (Memo plutusData sbs))

-- | It is safe to convert `BinaryData` to `Data` because the only way to
-- construct `BinaryData` is thorugh smart constructor `makeBinaryData` that
-- takes care of verification.
binaryDataToData :: BinaryData era -> Data era
binaryDataToData binaryData =
  case decodeBinaryData binaryData of
    Left errMsg ->
      error $ toText $ "Impossible: incorrectly encoded data:" ++ show errMsg
    Right d -> d

dataToBinaryData :: Data era -> BinaryData era
dataToBinaryData (DataConstr (Memo _ sbs)) = BinaryData sbs

--- End of cardano-ledger
