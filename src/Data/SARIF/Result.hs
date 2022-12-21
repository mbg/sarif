--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Result` type which represents results of a
-- static analysis tool.
module Data.SARIF.Result (
    Level(..),
    Result(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional hiding (Result, Error)
import Data.Text

import Data.SARIF.Level
import Data.SARIF.Location
import Data.SARIF.MultiformatMessageString

--------------------------------------------------------------------------------

-- | Represents the results of a run of a static analysis tool.
data Result = MkResult {
    -- | The unique ID of the rule of which this result is an occurrence of.
    resultRuleId :: Text,
    -- | A result-specific message which may refer to specific variable names
    -- etc. which caused the rule to trigger.
    resultMessage :: MultiformatMessageString,
    -- | A list of locations which caused the rule to trigger.
    resultLocations :: [Location],
    -- | An optional override for the default `Level` of the rule.
    resultLevel :: Maybe Level
} deriving (Eq, Show)

instance ToJSON Result where
    toJSON MkResult{..} = object
        [ "ruleId" .= resultRuleId
        , "message" .= resultMessage
        , "locations" .= resultLocations
        , "level" .=? resultLevel
        ]

instance FromJSON Result where
    parseJSON = withObject "Result" $ \obj ->
        MkResult <$> obj .: "ruleId"
                 <*> obj .: "message"
                 <*> obj .: "locations"
                 <*> obj .: "level"

--------------------------------------------------------------------------------
