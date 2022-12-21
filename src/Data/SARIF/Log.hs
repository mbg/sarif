--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the top-level structure of SARIF.
module Data.SARIF.Log (
    Log(..),
    defaultLog,
    decodeSarifFileStrict,
    encodeSarifAsLBS
) where

--------------------------------------------------------------------------------

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text

import Data.SARIF.Run (Run(..))

--------------------------------------------------------------------------------

-- | Each SARIF file contains one `Log` value at the top.
data Log = MkLog {
    -- | The version identifier of the SARIF format used by the file.
    logVersion :: Text,
    -- | A list of descriptions of runs of static analysis tools.
    logRuns :: [Run]
} deriving (Eq, Show)

-- | The URL of the JSON schema that describes SARIF.
schemaUrl :: Text
schemaUrl = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"

instance ToJSON Log where
    toJSON MkLog{..} = object
        [ "version" .= logVersion
        , "runs" .= logRuns
        , "$schema" .= schemaUrl
        ]

instance FromJSON Log where
    parseJSON = withObject "Log" $ \obj ->
        MkLog <$> obj .: "version"
              <*> obj .: "runs"

-- | Represents a default `Log` value.
defaultLog :: Log
defaultLog = MkLog{
    logVersion = "2.1.0",
    logRuns = []
}

-- | `decodeSarifFileStrict` @filepath@ is a type-specialised version of
-- `eitherDecodeFileStrict` for `Log`.
decodeSarifFileStrict :: FilePath -> IO (Either String Log)
decodeSarifFileStrict = eitherDecodeFileStrict

-- | `encodeSarifAsLBS` @log@ encodes a `Log` value as a lazy `LBS.ByteString`.
-- This is a type-specialised version of `encode`.
encodeSarifAsLBS :: Log -> LBS.ByteString
encodeSarifAsLBS = encode

--------------------------------------------------------------------------------
