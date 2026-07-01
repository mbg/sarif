--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the top-level structure of SARIF.
module Data.SARIF.Log
  ( Log (..),
    decodeSarifFileStrict,
    encodeSarifAsLBS,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import qualified Data.ByteString.Lazy as LBS
import Data.SARIF.ExternalPropertyFileReferences
import Data.SARIF.Run
import Data.Text

--------------------------------------------------------------------------------

-- | Each SARIF file contains one `Log` value at the top.
data Log = MkLog
  { -- | The version identifier of the SARIF format used by the file.
    logVersion :: Text,
    -- | An absolute URI to the JSON schema describing the SARIF format version of this log file.
    logSchema :: Maybe Text,
    -- | A list of descriptions of runs of static analysis tools.
    logRuns :: Maybe [Run],
    -- | The inlineExternalProperties property of the log
    logProperties :: Maybe ExternalPropertyFileReferences
  }
  deriving (Eq, Show, Ord)

instance ToJSON Log where
  toJSON MkLog {..} =
    object
      [ "version" .= logVersion,
        "runs" .=? logRuns,
        "$schema" .=? logSchema,
        "inlineExternalProperties" .=? logProperties
      ]

instance FromJSON Log where
  parseJSON = withObject "Log" $ \obj ->
    MkLog
      <$> obj .: "version"
      <*> obj .:? "$schema"
      <*> obj .:? "runs"
      <*> obj .:? "inlineExternalProperties"

-- | `decodeSarifFileStrict` @filepath@ is a type-specialised version of
-- `eitherDecodeFileStrict` for `Log`.
decodeSarifFileStrict :: FilePath -> IO (Either String Log)
decodeSarifFileStrict = eitherDecodeFileStrict

-- | `encodeSarifAsLBS` @log@ encodes a `Log` value as a lazy `LBS.ByteString`.
-- This is a type-specialised version of `encode`.
encodeSarifAsLBS :: Log -> LBS.ByteString
encodeSarifAsLBS = encode

--------------------------------------------------------------------------------
