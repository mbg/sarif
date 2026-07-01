--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Region
  ( Region (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactContent
import Data.SARIF.Message
import Data.Text

data Region = MkRegion
  { -- | The startLine property of a region object
    regionStartLine :: Maybe Int,
    -- | The startColumn property of a region object
    regionStartColumn :: Maybe Int,
    -- | The endLine property of a region object
    regionEndLine :: Maybe Int,
    -- | The endColumn property of a region object
    regionEndColumn :: Maybe Int,
    -- | The charOffset property of a region object
    regionCharOffset :: Maybe Int,
    -- | The charLength property of a region object
    regionCharLength :: Maybe Int,
    -- | The byteOffset property of a region object
    regionByteOffset :: Maybe Int,
    -- | The byteLength property of a region object
    regionByteLength :: Maybe Int,
    -- | The snippet property of a region object
    regionSnippet :: Maybe ArtifactContent,
    -- | The message property of a region object
    regionMessage :: Maybe Message,
    -- | The sourceLanguage property of a region object
    regionSourceLanguage :: Maybe Text,
    -- | The properties property of the Region object
    regionProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Region where
  toJSON MkRegion {..} =
    object
      [ "startLine" .=? regionStartLine,
        "startColumn" .=? regionStartColumn,
        "endLine" .=? regionEndLine,
        "endColumn" .=? regionEndColumn,
        "charOffset" .=? regionCharOffset,
        "charLength" .=? regionCharLength,
        "byteOffset" .=? regionByteOffset,
        "byteLength" .=? regionByteLength,
        "snippet" .=? regionSnippet,
        "message" .=? regionMessage,
        "sourceLanguage" .=? regionSourceLanguage,
        "properties" .=? regionProperties
      ]

instance FromJSON Region where
  parseJSON = withObject "Region" $ \obj ->
    MkRegion
      <$> obj .:? "startLine"
      <*> obj .:? "startColumn"
      <*> obj .:? "endLine"
      <*> obj .:? "endColumn"
      <*> obj .:? "charOffset"
      <*> obj .:? "charLength"
      <*> obj .:? "byteOffset"
      <*> obj .:? "byteLength"
      <*> obj .:? "snippet"
      <*> obj .:? "message"
      <*> obj .:? "sourceLanguage"
      <*> obj .:? "properties"
