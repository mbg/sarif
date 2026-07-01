--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Artifact` type to represent SARIF artifacts.
module Data.SARIF.Artifact
  ( Artifact (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactContent
import Data.SARIF.ArtifactLocation
import Data.SARIF.Message
import Data.Text

--------------------------------------------------------------------------------

data ArtifactRole
  = ArtifactAnalysisTargetRole
  | ArtifactAttachmentRole
  | ArtifactConversionSourceRole
  | ArtifactDebugOutputFileRole
  | ArtifactDirectoryRole
  | ArtifactDriverRole
  | ArtifactExtensionRole
  | ArtifactExternalPropertyFileRole
  | ArtifactMemoryContentsRole
  | ArtifactPolicyRole
  | ArtifactReferencedOnCommandLineRole
  | ArtifactRepositoryRootRole
  | ArtifactResponseFileRole
  | ArtifactResultFileRole
  | ArtifactStandardStreamRole
  | ArtifactTaxonomyRole
  | ArtifactToolSpecifiedConfigurationRole
  | ArtifactTracedFileRole
  | ArtifactTranslationRole
  | ArtifactUserSpecifiedConfigurationRole
  | ArtifactAddedRole
  | ArtifactDeletedRole
  | ArtifiedModifiedRole
  | ArtifactRenamedRole
  | ArtifactUncontrolledRole
  | ArtifactUnmodifiedRole
  deriving (Eq, Show, Ord)

instance ToJSON ArtifactRole where
  toJSON ArtifactAnalysisTargetRole = String "analysisTarget"
  toJSON ArtifactAttachmentRole = String "attachment"
  toJSON ArtifactConversionSourceRole = String "conversionSource"
  toJSON ArtifactDebugOutputFileRole = String "debugOutputFile"
  toJSON ArtifactDirectoryRole = String "directory"
  toJSON ArtifactDriverRole = String "driver"
  toJSON ArtifactExtensionRole = String "extension"
  toJSON ArtifactExternalPropertyFileRole = String "externalPropertyFile"
  toJSON ArtifactMemoryContentsRole = String "memoryContents"
  toJSON ArtifactPolicyRole = String "policy"
  toJSON ArtifactReferencedOnCommandLineRole = String "referencedOnCommandLine"
  toJSON ArtifactRepositoryRootRole = String "repositoryRoot"
  toJSON ArtifactResponseFileRole = String "responseFile"
  toJSON ArtifactResultFileRole = String "resultFile"
  toJSON ArtifactStandardStreamRole = String "standardStream"
  toJSON ArtifactTaxonomyRole = String "taxonomy"
  toJSON ArtifactToolSpecifiedConfigurationRole = String "toolSpecifiedConfiguration"
  toJSON ArtifactTracedFileRole = String "tracedFile"
  toJSON ArtifactTranslationRole = String "translation"
  toJSON ArtifactUserSpecifiedConfigurationRole = String "userSpecifiedConfiguration"
  toJSON ArtifactAddedRole = String "added"
  toJSON ArtifactDeletedRole = String "deleted"
  toJSON ArtifiedModifiedRole = String "modified"
  toJSON ArtifactRenamedRole = String "renamed"
  toJSON ArtifactUncontrolledRole = String "uncontrolled"
  toJSON ArtifactUnmodifiedRole = String "unmodified"

instance FromJSON ArtifactRole where
  parseJSON = withText "ArtifactRole" $ \case
    "analysisTarget" -> pure ArtifactAnalysisTargetRole
    "attachment" -> pure ArtifactAttachmentRole
    "conversionSource" -> pure ArtifactConversionSourceRole
    "debugOutputFile" -> pure ArtifactDebugOutputFileRole
    "directory" -> pure ArtifactDirectoryRole
    "driver" -> pure ArtifactDriverRole
    "extension" -> pure ArtifactExtensionRole
    "externalPropertyFile" -> pure ArtifactExternalPropertyFileRole
    "memoryContents" -> pure ArtifactMemoryContentsRole
    "policy" -> pure ArtifactPolicyRole
    "referencedOnCommandLine" -> pure ArtifactReferencedOnCommandLineRole
    "repositoryRoot" -> pure ArtifactRepositoryRootRole
    "responseFile" -> pure ArtifactResponseFileRole
    "resultFile" -> pure ArtifactResultFileRole
    "standardStream" -> pure ArtifactStandardStreamRole
    "taxonomy" -> pure ArtifactTaxonomyRole
    "toolSpecifiedConfiguration" -> pure ArtifactToolSpecifiedConfigurationRole
    "tracedFile" -> pure ArtifactTracedFileRole
    "translation" -> pure ArtifactTranslationRole
    "userSpecifiedConfiguration" -> pure ArtifactUserSpecifiedConfigurationRole
    "added" -> pure ArtifactAddedRole
    "deleted" -> pure ArtifactDeletedRole
    "modified" -> pure ArtifiedModifiedRole
    "renamed" -> pure ArtifactRenamedRole
    "uncontrolled" -> pure ArtifactUncontrolledRole
    "unmodified" -> pure ArtifactUnmodifiedRole
    other -> fail $ "Unknown ArtifactRole: " <> unpack other

data Artifact = MkArtifact
  { -- | The location where the artifact was found.
    artifactLocation :: Maybe ArtifactLocation,
    -- | The parentIndex property of the artifact.
    artifactParentIndex :: Maybe Int,
    -- | The offset property of the artifact.
    artifactOffset :: Maybe Int,
    -- | The length property of the artifact.
    artifactLength :: Maybe Int,
    -- | The roles property of the artifact.
    artifactRoles :: Maybe [ArtifactRole],
    -- | The mimeType property of the artifact.
    artifactMimeType :: Maybe Text,
    -- | The contents property of the artifact.
    artifactContents :: Maybe ArtifactContent,
    -- | The encoding property of the artifact.
    artifactEncoding :: Maybe Text,
    -- | The sourceLanguage property of the artifact.
    artifactSourceLanguage :: Maybe Text,
    -- | The hashes property of the artifact.
    artifactHashes :: Maybe [(Text, Text)],
    -- | The lastModifiedTimeUtc property of the artifact.
    artifactLastModifiedTimeUtc :: Maybe Text,
    -- | The description property of the artifact.
    artifactDescription :: Maybe Message,
    -- | The properties property of an Artifact object
    artifactProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Artifact where
  toJSON MkArtifact {..} =
    object
      [ "location" .=? artifactLocation,
        "parentIndex" .=? artifactParentIndex,
        "offset" .=? artifactOffset,
        "length" .=? artifactLength,
        "roles" .=? artifactRoles,
        "mimeType" .=? artifactMimeType,
        "contents" .=? artifactContents,
        "encoding" .=? artifactEncoding,
        "sourceLanguage" .=? artifactSourceLanguage,
        "hashes" .=? artifactHashes,
        "lastModifiedTimeUtc" .=? artifactLastModifiedTimeUtc,
        "description" .=? artifactDescription,
        "properties" .=? artifactProperties
      ]

instance FromJSON Artifact where
  parseJSON = withObject "Artifact" $ \v ->
    MkArtifact
      <$> v .:? "location"
      <*> v .:? "parentIndex"
      <*> v .:? "offset"
      <*> v .:? "length"
      <*> v .:? "roles"
      <*> v .:? "mimeType"
      <*> v .:? "contents"
      <*> v .:? "encoding"
      <*> v .:? "sourceLanguage"
      <*> v .:? "hashes"
      <*> v .:? "lastModifiedTimeUtc"
      <*> v .:? "description"
      <*> v .:? "properties"

-- --------------------------------------------------------------------------------
