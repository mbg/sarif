--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Fix where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactChange
import Data.SARIF.Message
import Data.Text

data Fix = MkFix
  { -- | The description property of the fix
    fixDescription :: Message,
    -- | The artifactChanges property of the fix
    fixArtifactChanges :: [ArtifactChange],
    -- | The properties property of the Fix object
    fixProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Fix where
  toJSON MkFix {..} =
    object
      [ "description" .= fixDescription,
        "artifactChanges" .= fixArtifactChanges,
        "properties" .=? fixProperties
      ]

instance FromJSON Fix where
  parseJSON = withObject "Fix" $ \obj ->
    MkFix
      <$> obj .: "description"
      <*> obj .: "artifactChanges"
      <*> obj .:? "properties"
