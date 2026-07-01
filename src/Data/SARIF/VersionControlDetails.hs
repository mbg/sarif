--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.VersionControlDetails (VersionControlDetails (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.Text

data VersionControlDetails = MkVersionControlDetails
  { -- | The repositoryUri property of the version control details
    versionControlDetailsRepositoryUri :: Text,
    -- | The revisionId property of the version control details
    versionControlDetailsRevisionId :: Text,
    -- | The branch property of the version control details
    versionControlDetailsBranch :: Maybe Text,
    -- | The revisionTag property of the version control details
    versionControlDetailsRevisionTag :: Maybe Text,
    -- | The asOfTimeUtc property of the version control details
    versionControlDetailsAsOfTimeUtc :: Maybe Text,
    -- | The mappedTo property of the version control details
    versionControlDetailsMappedTo :: Maybe ArtifactLocation,
    -- | The properties property of a VersionControlDetails object
    versionControlDetailsProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON VersionControlDetails where
  toJSON MkVersionControlDetails {..} =
    object
      [ "repositoryUri" .= versionControlDetailsRepositoryUri,
        "revisionId" .= versionControlDetailsRevisionId,
        "branch" .=? versionControlDetailsBranch,
        "revisionTag" .=? versionControlDetailsRevisionTag,
        "asOfTimeUtc" .=? versionControlDetailsAsOfTimeUtc,
        "mappedTo" .=? versionControlDetailsMappedTo,
        "properties" .=? versionControlDetailsProperties
      ]

instance FromJSON VersionControlDetails where
  parseJSON = withObject "VersionControlDetails" $ \obj ->
    MkVersionControlDetails
      <$> obj .: "repositoryUri"
      <*> obj .: "revisionId"
      <*> obj .:? "branch"
      <*> obj .:? "revisionTag"
      <*> obj .:? "asOfTimeUtc"
      <*> obj .:? "mappedTo"
      <*> obj .:? "properties"
