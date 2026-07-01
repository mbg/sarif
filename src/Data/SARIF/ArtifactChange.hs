module Data.SARIF.ArtifactChange where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.SARIF.Replacement
import Data.Text

data ArtifactChange = MkArtifactChange
  { -- | The artifactLocation property of the artifact change
    artifactChangeArtifactLocation :: ArtifactLocation,
    -- | The replacement property of the artifact change
    artifactChangeReplacementArtifacts :: [Replacement],
    -- | The properties property of an ArtifactChange object
    artifactChangeProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ArtifactChange where
  toJSON MkArtifactChange {..} =
    object
      [ "artifactLocation" .= artifactChangeArtifactLocation,
        "replacements" .= artifactChangeReplacementArtifacts,
        "properties" .=? artifactChangeProperties
      ]

instance FromJSON ArtifactChange where
  parseJSON = withObject "ArtifactChange" $ \obj ->
    MkArtifactChange
      <$> obj .: "artifactLocation"
      <*> obj .: "replacements"
      <*> obj .:? "properties"