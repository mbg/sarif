--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.ToolComponent (ToolComponent (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.SARIF.MultiformatMessageString
import Data.SARIF.ReportingDescriptor
import Data.SARIF.ToolComponentReference
import Data.SARIF.TranslationMetadata
import Data.Text
import Data.Time
import Data.UUID.Types

data ToolComponentContent
  = ToolComponentLocalizedDataContent
  | ToolComponentContentNonLocalizedData
  deriving (Eq, Show, Ord)

instance ToJSON ToolComponentContent where
  toJSON ToolComponentLocalizedDataContent = String "localizedData"
  toJSON ToolComponentContentNonLocalizedData = String "nonLocalizedData"

instance FromJSON ToolComponentContent where
  parseJSON = withText "ToolComponentContent" $ \case
    "localizedData" -> pure ToolComponentLocalizedDataContent
    "nonLocalizedData" -> pure ToolComponentContentNonLocalizedData
    _ -> fail "Unknown ToolComponentContent"

-- | A description of a tool component, such as a static analysis tool or
-- an extension to one.
data ToolComponent = MkToolComponent
  { -- | A GUID which identifies the tool component.
    toolComponentGUID :: Maybe UUID,
    -- | The short name of the tool component.
    toolComponentName :: Maybe Text,
    -- | The full name of the tool component.
    toolComponentFullName :: Maybe Text,
    -- | The name of the product to which the tool component belongs.
    toolComponentProduct :: Maybe Text,
    -- | The name of the suite of products to which the tool component belongs.
    toolComponentProductSuit :: Maybe Text,
    -- | The semver of the tool component.
    toolComponentSemanticVersion :: Maybe Text,
    -- | The version of the tool component.
    toolComponentVersion :: Maybe Text,
    -- | The UTC date and time at which the tool component was released.
    toolComponentReleaseDateUtc :: Maybe UTCTime,
    -- | A URI for downloading this tool component version.
    toolComponentDownloadUri :: Maybe Text,
    -- | A URI pointing to more information about the tool component.
    toolComponentInformationUri :: Maybe Text,
    -- | The name of the company or organization that produced the tool componen
    toolComponentOrganization :: Maybe Text,
    -- | One sentence description of the tool component.
    toolComponentShortDescription :: Maybe MultiformatMessageString,
    -- | A full description of the tool component.
    toolComponentFullDescription :: Maybe MultiformatMessageString,
    -- | the language of the string contained in the component
    toolComponentLanguage :: Maybe Text,
    -- | Mapping from Message ID to its corresponding message string.
    toolComponentGlobalMessageStrings :: Maybe (Map Text MultiformatMessageString),
    -- | A list of rules that the tool component applies.
    toolComponentRules :: Maybe [ReportingDescriptor],
    -- | A list of notifications information provided by the tool component
    toolComponentNotifications :: Maybe [ReportingDescriptor],
    -- | A list of taxonomies that
    toolComponentTaxa :: Maybe [ReportingDescriptor],
    -- | The supportedTaxonomies property of the tool component
    toolComponentSupportedTaxonomies :: Maybe [ToolComponentReference],
    -- | The translationMetadata property of the tool component
    toolComponentTranslationMetadata :: Maybe TranslationMetadata,
    -- | The locations property of the tool component
    toolComponentLocations :: Maybe [ArtifactLocation],
    -- | The contents property of the tool component
    toolComponentContents :: Maybe [Text],
    -- | The isComprehensive property of the tool component
    toolComponentIsComprehensive :: Maybe Bool,
    -- | The localizedDataSemanticVersion property of the tool component
    toolComponentLocalizedDataSemanticVersion :: Maybe Text,
    -- | The minimumRequiredLocalizedDataSemanticVersion property of the tool component
    toolComponentMinimumRequiredLocalizedDataSemanticVersion :: Maybe Text,
    -- | The associatedComponent property of the tool component
    toolComponentAssociatedComponent :: Maybe ToolComponentReference,
    -- | The properties property of a ToolComponent object
    toolComponentProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ToolComponent where
  toJSON MkToolComponent {..} =
    object
      [ "guid" .=? toolComponentGUID,
        "name" .= toolComponentName,
        "fullName" .=? toolComponentFullName,
        "semanticVersion" .=? toolComponentSemanticVersion,
        "version" .=? toolComponentVersion,
        "releaseDateUtc" .=? toolComponentReleaseDateUtc,
        "downloadUri" .=? toolComponentDownloadUri,
        "informationUri" .=? toolComponentInformationUri,
        "organization" .=? toolComponentOrganization,
        "shortDescription" .=? toolComponentShortDescription,
        "fullDescription" .=? toolComponentFullDescription,
        "language" .=? toolComponentLanguage,
        "globalMessageStrings" .=? toolComponentGlobalMessageStrings,
        "rules" .=? toolComponentRules,
        "notifications" .=? toolComponentNotifications,
        "taxa" .=? toolComponentTaxa,
        "supportedTaxonomies" .=? toolComponentSupportedTaxonomies,
        "translationMetadata" .=? toolComponentTranslationMetadata,
        "locations" .=? toolComponentLocations,
        "contents" .=? toolComponentContents,
        "isComprehensive" .=? toolComponentIsComprehensive,
        "localizedDataSemanticVersion" .=? toolComponentLocalizedDataSemanticVersion,
        "minimumRequiredLocalizedDataSemanticVersion"
          .=? toolComponentMinimumRequiredLocalizedDataSemanticVersion,
        "associatedComponent"
          .=? toolComponentAssociatedComponent,
        "properties" .=? toolComponentProperties
      ]

instance FromJSON ToolComponent where
  parseJSON = withObject "ToolComponent" $ \obj ->
    MkToolComponent
      <$> obj .:? "guid"
      <*> obj .:? "name"
      <*> obj .:? "fullName"
      <*> obj .:? "product"
      <*> obj .:? "productSuite"
      <*> obj .:? "semanticVersion"
      <*> obj .:? "version"
      <*> obj .:? "releaseDateUtc"
      <*> obj .:? "downloadUri"
      <*> obj .:? "informationUri"
      <*> obj .:? "organization"
      <*> obj .:? "shortDescription"
      <*> obj .:? "fullDescription"
      <*> obj .:? "language"
      <*> obj .:? "globalMessageStrings"
      <*> obj .:? "rules"
      <*> obj .:? "notifications"
      <*> obj .:? "taxa"
      <*> obj .:? "supportedTaxonomies"
      <*> obj .:? "translationMetadata"
      <*> obj .:? "locations"
      <*> obj .:? "contents"
      <*> obj .:? "isComprehensive"
      <*> obj .:? "localizedDataSemanticVersion"
      <*> obj .:? "minimumRequiredLocalizedDataSemanticVersion"
      <*> obj .:? "associatedComponent"
      <*> obj .:? "properties"
