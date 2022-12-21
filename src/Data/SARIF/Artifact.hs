--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Artifact` type to represent SARIF artifacts.
module Data.SARIF.Artifact (
    Artifact(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Text

import Data.SARIF.Location

--------------------------------------------------------------------------------

-- | An artifact represents e.g. a source file.
data Artifact = MkArtifact {
    -- | The location where the artifact was found.
    artifactLocation :: ArtifactLocation,
    -- | The mime type of the artifact.
    artifactMimeType :: Maybe Text
} deriving (Eq, Show)

instance ToJSON Artifact where
    toJSON MkArtifact{..} = object
        [ "location" .= artifactLocation
        , "mimeType" .=? artifactMimeType
        ]

instance FromJSON Artifact where
    parseJSON = withObject "Artifact" $ \obj ->
        MkArtifact <$> obj .: "location"
                   <*> obj .:? "mimeType"

--------------------------------------------------------------------------------
