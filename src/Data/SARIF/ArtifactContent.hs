--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.ArtifactContent
  ( ArtifactContent (..),
  )
where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.MultiformatMessageString
import Data.Text

data ArtifactContent = MkArtifactContent
  { -- | The text property of an artifactContent object
    artifactContentText :: Maybe Text,
    -- | The binary property of an artifactContent object
    artifactContentBinary :: Maybe Text,
    -- | The rendered property of an artifactContent object
    artifactContentRendered :: Maybe MultiformatMessageString,
    -- | The properties property of an ArtifactContent object
    artifactContentProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ArtifactContent where
  toJSON MkArtifactContent {..} =
    object
      [ "text" .=? artifactContentText,
        "binary" .=? artifactContentBinary,
        "rendered" .=? artifactContentRendered,
        "properties" .=? artifactContentProperties
      ]

instance FromJSON ArtifactContent where
  parseJSON = withObject "ArtifactContent" $ \obj ->
    MkArtifactContent
      <$> obj .: "text"
      <*> obj .:? "binary"
      <*> obj .:? "rendered"
      <*> obj .:? "properties"
