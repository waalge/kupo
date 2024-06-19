--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Kupo.Data.Http.JoinScripts
    ( JoinScripts (..)
    , joinScriptsFromQueryParams
    ) where

import Kupo.Prelude

import qualified Network.HTTP.Types.URI as Http

data JoinScripts = NoJoinScripts | JoinScripts
    deriving (Show)

joinScriptsFromQueryParams
    :: Http.Query
    -> Maybe JoinScripts
joinScriptsFromQueryParams = \case
    ("join_scripts", val):_ -> do
        if val == Just "false"
            then Just NoJoinScripts
            else Just JoinScripts
    [] ->
        Just NoJoinScripts
    _:rest ->
        joinScriptsFromQueryParams rest
