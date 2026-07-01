--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.ArtifactLocation
  ( ArtifactLocation (..),
  )
where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.Text

data ArtifactLocation = MkArtifactLocation
  { -- | The uri property of an artifactLocation object
    artifactLocUri :: Maybe Text,
    -- | The uriBasedId property of an artifactLocation object
    artifactLocUriBaseId :: Maybe Text,
    -- | The index property of an artifactLocation object
    artifactLocIndex :: Maybe Int,
    -- | The description property of an artifactLocation object
    artifactLocDescription :: Maybe Message,
    -- | The properties property of an ArtifactLocation object
    artifactLocProperties :: Maybe (Map Text Value)
  }
  deriving (Show)

instance Eq ArtifactLocation where
  (==) a b =
    artifactLocUri a == artifactLocUri b
      && artifactLocUriBaseId a == artifactLocUriBaseId b
      && artifactLocDescription a == artifactLocDescription b
      && artifactLocProperties a == artifactLocProperties b

instance Ord ArtifactLocation where
  compare a b =
    compare (artifactLocUri a) (artifactLocUri b)
      <> compare (artifactLocUriBaseId a) (artifactLocUriBaseId b)
      <> compare (artifactLocDescription a) (artifactLocDescription b)
      <> compare (artifactLocProperties a) (artifactLocProperties b)

instance ToJSON ArtifactLocation where
  toJSON MkArtifactLocation {..} =
    object
      [ "uri" .=? artifactLocUri,
        "uriBaseId" .=? artifactLocUriBaseId,
        "index" .=? artifactLocIndex,
        "description" .=? artifactLocDescription,
        "properties" .=? artifactLocProperties
      ]

instance FromJSON ArtifactLocation where
  parseJSON = withObject "ArtifactLocation" $ \obj ->
    MkArtifactLocation
      <$> (obj .:? "uri")
      <*> (obj .:? "uriBaseId")
      <*> obj .:? "index"
      <*> obj .:? "description"
      <*> obj .:? "properties"
