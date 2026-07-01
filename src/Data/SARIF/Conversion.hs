--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Conversion where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.SARIF.Invocation
import Data.SARIF.Tool
import Data.Text

data Conversion = MkConversion
  { -- | The tool property of the conversion
    conversionTool :: Tool,
    -- | The invocation property of the conversion
    conversionInvocation :: Maybe Invocation,
    -- | The analysisToolLogFiles property of the conversion
    conversionAnalysisToolLogFiles :: Maybe [ArtifactLocation],
    -- | The properties property of the Conversion object
    conversionProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Conversion where
  toJSON MkConversion {..} =
    object
      [ "tool" .= conversionTool,
        "invocation" .=? conversionInvocation,
        "analysisToolLogFiles" .=? conversionAnalysisToolLogFiles,
        "properties" .=? conversionProperties
      ]

instance FromJSON Conversion where
  parseJSON = withObject "Conversion" $ \obj ->
    MkConversion
      <$> obj .: "tool"
      <*> obj .:? "invocation"
      <*> obj .:? "analysisToolLogFiles"
      <*> obj .:? "properties"