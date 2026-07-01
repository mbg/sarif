--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ResultProvenance where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.PhysicalLocation
import Data.Text

data ResultProvenance = MkResultProvenance
  { -- | The firstDetectionTimeUtc property of the result provenance
    resultProvenanceFirstDetectionTimeUtc :: Maybe Text,
    -- | The lastDetectionTimeUtc property of the result provenance
    resultProvenanceLastDetectionTimeUtc :: Maybe Text,
    -- | The firstDetectionRunGuid property of the result provenance
    resultProvenanceFirstDetectionRunGuid :: Maybe Text,
    -- | The lastDetectionRunGuid property of the result provenance
    resultProvenanceLastDetectionRunGuid :: Maybe Text,
    -- | The invocationIndex property of the result provenance
    resultProvenanceInvocationIndex :: Maybe Int,
    -- | The conversionSources property of the result provenance
    resultProvenanceConversionSources :: Maybe [PhysicalLocation],
    -- | The properties property of the ResultProvenance object
    resultProvenanceProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ResultProvenance where
  toJSON MkResultProvenance {..} =
    object
      [ "firstDetectionTimeUtc" .=? resultProvenanceFirstDetectionTimeUtc,
        "lastDetectionTimeUtc" .=? resultProvenanceLastDetectionTimeUtc,
        "firstDetectionRunGuid" .=? resultProvenanceFirstDetectionRunGuid,
        "lastDetectionRunGuid" .=? resultProvenanceLastDetectionRunGuid,
        "invocationIndex" .=? resultProvenanceInvocationIndex,
        "conversionSources" .=? resultProvenanceConversionSources,
        "properties" .=? resultProvenanceProperties
      ]

instance FromJSON ResultProvenance where
  parseJSON = withObject "ResultProvenance" $ \obj ->
    MkResultProvenance
      <$> obj .:? "firstDetectionTimeUtc"
      <*> obj .:? "lastDetectionTimeUtc"
      <*> obj .:? "firstDetectionRunGuid"
      <*> obj .:? "lastDetectionRunGuid"
      <*> obj .:? "invocationIndex"
      <*> obj .:? "conversionSources"
      <*> obj .:? "properties"
