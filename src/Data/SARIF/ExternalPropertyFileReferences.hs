--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ExternalPropertyFileReferences (ExternalPropertyFileReferences (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ExternalPropertyFileReference
import Data.Text

data ExternalPropertyFileReferences = MkExternalPropertyFileReferences
  { -- | The addresses property of the external property file references
    externalPropertyFileReferencesAddresses :: Maybe [ExternalPropertyFileReference],
    -- | The artifacts property of the external property file references
    externalPropertyFileReferencesArtifacts :: Maybe [ExternalPropertyFileReference],
    -- | The conversion property of the external property file references
    externalPropertyFileReferencesConversion :: Maybe ExternalPropertyFileReference,
    -- | The graphs property of the external property file references
    externalPropertyFileReferencesGraphs :: Maybe [ExternalPropertyFileReference],
    -- | The invocations property of the external property file references
    externalPropertyFileReferencesInvocations :: Maybe [ExternalPropertyFileReference],
    -- | The logicalLocations property of the external property file references
    externalPropertyFileReferencesLogicalLocations :: Maybe [ExternalPropertyFileReference],
    -- | The policies property of the external property file references
    externalPropertyFileReferencesPolicies :: Maybe [ExternalPropertyFileReference],
    -- | The externalizedProperties property of the external property file references
    externalPropertyFileReferencesExternalizedProperties :: Maybe ExternalPropertyFileReference,
    -- | The webRequests property of the external property file references
    externalPropertyFileReferencesWebRequests :: Maybe [ExternalPropertyFileReference],
    -- | The webResponses property of the external property file references
    externalPropertyFileReferencesWebResponses :: Maybe [ExternalPropertyFileReference],
    -- | The results property of the external property file references
    externalPropertyFileReferencesResults :: Maybe [ExternalPropertyFileReference],
    -- | The taxonomies property of the external property file references
    externalPropertyFileReferencesTaxonomies :: Maybe [ExternalPropertyFileReference],
    -- | The threadFlowLocations property of the external property file references
    externalPropertyFileReferencesThreadFlowLocations :: Maybe [ExternalPropertyFileReference],
    -- | The translations property of the external property file references
    externalPropertyFileReferencesTranslations :: Maybe [ExternalPropertyFileReference],
    -- | The driver property of the external property file references
    externalPropertyFileReferencesDriver :: Maybe ExternalPropertyFileReference,
    -- | The extensions property of the external property file references
    externalPropertyFileReferencesExtensions :: Maybe [ExternalPropertyFileReference],
    -- | The properties property of the ExternalPropertyFileReferences object
    externalPropertyFileReferencesProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ExternalPropertyFileReferences where
  toJSON MkExternalPropertyFileReferences {..} =
    object
      [ "addresses" .=? externalPropertyFileReferencesAddresses,
        "artifacts" .=? externalPropertyFileReferencesArtifacts,
        "conversion" .=? externalPropertyFileReferencesConversion,
        "graphs" .=? externalPropertyFileReferencesGraphs,
        "invocations" .=? externalPropertyFileReferencesInvocations,
        "logicalLocations" .=? externalPropertyFileReferencesLogicalLocations,
        "policies" .=? externalPropertyFileReferencesPolicies,
        "externalizedProperties" .=? externalPropertyFileReferencesExternalizedProperties,
        "webRequests" .=? externalPropertyFileReferencesWebRequests,
        "webResponses" .=? externalPropertyFileReferencesWebResponses,
        "results" .=? externalPropertyFileReferencesResults,
        "taxonomies" .=? externalPropertyFileReferencesTaxonomies,
        "threadFlowLocations" .=? externalPropertyFileReferencesThreadFlowLocations,
        "translations" .=? externalPropertyFileReferencesTranslations,
        "driver" .=? externalPropertyFileReferencesDriver,
        "extensions" .=? externalPropertyFileReferencesExtensions,
        "properties" .=? externalPropertyFileReferencesProperties
      ]

instance FromJSON ExternalPropertyFileReferences where
  parseJSON = withObject "ExternalPropertyFileReferences" $ \obj ->
    MkExternalPropertyFileReferences
      <$> obj .:? "addresses"
      <*> obj .:? "artifacts"
      <*> obj .:? "conversion"
      <*> obj .:? "graphs"
      <*> obj .:? "invocations"
      <*> obj .:? "logicalLocations"
      <*> obj .:? "policies"
      <*> obj .:? "externalizedProperties"
      <*> obj .:? "webRequests"
      <*> obj .:? "webResponses"
      <*> obj .:? "results"
      <*> obj .:? "taxonomies"
      <*> obj .:? "threadFlowLocations"
      <*> obj .:? "translations"
      <*> obj .:? "driver"
      <*> obj .:? "extensions"
      <*> obj .:? "properties"
