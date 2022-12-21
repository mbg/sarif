--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides types used to refer to different sorts of locations, such as
-- files, as well as parts of files.
module Data.SARIF.Location (
    Location(..),
    ArtifactLocation(..),
    Region(..),
    PhysicalLocation(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Text

--------------------------------------------------------------------------------

-- | Represents locations.
newtype Location = MkLocation {
    -- | The physical location (i.e. part of a file) to which this location
    -- refers to.
    locationPhysicalLocation :: Maybe PhysicalLocation
} deriving (Eq, Show)

instance ToJSON Location where
    toJSON MkLocation{..} = object
        [ "physicalLocation" .= locationPhysicalLocation
        ]

instance FromJSON Location where
    parseJSON = withObject "Location" $ \obj ->
        MkLocation <$> obj .:? "physicalLocation"

-- | Represents artifact locations such as file paths.
newtype ArtifactLocation = MkArtifactLocation {
    artifactLocationUri :: Text
} deriving (Eq, Show)

instance ToJSON ArtifactLocation where
    toJSON MkArtifactLocation{..} = object
        [ "uri" .= artifactLocationUri
        ]

instance FromJSON ArtifactLocation where
    parseJSON = withObject "ArtifactLocation" $ \obj ->
        MkArtifactLocation <$> obj .: "uri"

-- | Represents regions of code with a start and an end.
data Region = MkRegion {
    -- | The line on which this region starts.
    regionStartLine :: Int,
    -- | The column within the starting line where this region starts.
    regionStartColumn :: Int,
    -- | The line on which this region ends.
    regionEndLine :: Int,
    -- | The column within the ending line where this region ends.
    regionEndColumn :: Int
} deriving (Eq, Show)

instance ToJSON Region where
    toJSON MkRegion{..} = object
        [ "startLine" .= regionStartLine
        , "startColumn" .= regionStartColumn
        , "endLine" .= regionEndLine
        , "endColumn" .= regionEndColumn
        ]

instance FromJSON Region where
    parseJSON = withObject "Region" $ \obj ->
        MkRegion <$> obj .: "startLine"
                 <*> obj .: "startColumn"
                 <*> obj .: "endLine"
                 <*> obj .: "endColumn"

-- | Represents parts of artifacts, e.g. a `Region` within a file.
data PhysicalLocation = MkPhysicalLocation {
    physicalLocationArtifactLocation :: ArtifactLocation,
    physicalLocationRegion :: Region
} deriving (Eq, Show)

instance ToJSON PhysicalLocation where
    toJSON MkPhysicalLocation{..} = object
        [ "artifactLocation" .= physicalLocationArtifactLocation
        , "region" .= physicalLocationRegion
        ]

instance FromJSON PhysicalLocation where
    parseJSON = withObject "PhysicalLocation" $ \obj ->
        MkPhysicalLocation <$> obj .: "artifactLocation"
                           <*> obj .: "region"

--------------------------------------------------------------------------------
