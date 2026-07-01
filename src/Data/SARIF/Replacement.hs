--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Replacement where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactContent
import Data.SARIF.Region
import Data.Text

data Replacement = MkReplacement
  { -- | The deletedRegion property of the replacement
    replacementDeletedRegion :: Region,
    -- | The insertedContent property of the replacement
    replacementInsertedContent :: Maybe ArtifactContent,
    -- | The properties property of the Replacement object
    replacementProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Replacement where
  toJSON MkReplacement {..} =
    object
      [ "deletedRegion" .= replacementDeletedRegion,
        "insertedContent" .=? replacementInsertedContent,
        "properties" .=? replacementProperties
      ]

instance FromJSON Replacement where
  parseJSON = withObject "Replacement" $ \obj ->
    MkReplacement
      <$> obj .: "deletedRegion"
      <*> obj .:? "insertedContent"
      <*> obj .:? "properties"