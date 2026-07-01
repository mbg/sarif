--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SARIF.ReportingDescriptor
  ( ReportingDescriptor (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.MultiformatMessageString
import Data.SARIF.ReportingConfiguration
import Data.SARIF.ReportingDescriptorRelationship
import Data.Text
import Data.UUID.Types

data ReportingDescriptor = MkReportingDescriptor
  { -- | The unique ID of the rule.
    reportingDescriptorId :: Text,
    -- | Previously used IDs for the rule.
    reportingDescriptorDeprecatedIds :: Maybe [Text],
    -- | A GUID which identifies the ReportingDescriptor.
    reportingDescriptorGuid :: Maybe UUID,
    -- | A list of deprecated GUIDs which previously identified the rule.
    reportingDescriptorDeprecatedGuids :: Maybe [UUID],
    -- | A friendly name for the rule, which should be one word in
    -- upper camel case.
    reportingDescriptorName :: Maybe Text,
    -- | A list of names for the rule that were previously used.
    reportingDescriptorDeprecatedNames :: Maybe [Text],
    -- | A short description for the rule, which may contain spaces
    -- and symbols.
    reportingDescriptorShortDescription :: Maybe MultiformatMessageString,
    -- | The full description.
    reportingDescriptorFullDescription :: Maybe MultiformatMessageString,
    -- | A mapping from arbitrary name to its corresponding message string.
    reportingDescriptorMessageStrings :: Maybe (Map Text MultiformatMessageString),
    -- | A URL to some help for this rule.
    reportingDescriptorHelpUri :: Maybe Text,
    -- | A recommendation for what to do in order to resolve an occurrence
    -- of this rule.
    reportingDescriptorHelp :: Maybe MultiformatMessageString,
    -- | The default reporting configuration for the rule.
    reportingDescriptorDefaultConfiguration :: Maybe ReportingConfiguration,
    -- | Extra properties for this rule.
    reportingDescriptorRelationships :: Maybe ReportingDescriptorRelationship,
    -- | The properties property of the ReportingDescriptor object
    reportingDescriptorProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ReportingDescriptor where
  toJSON MkReportingDescriptor {..} =
    object
      [ "id" .= reportingDescriptorId,
        "deprecatedIds" .=? reportingDescriptorDeprecatedIds,
        "guid" .=? reportingDescriptorGuid,
        "deprecatedGuids" .=? reportingDescriptorDeprecatedGuids,
        "name" .=? reportingDescriptorName,
        "deprecatedNames" .=? reportingDescriptorDeprecatedNames,
        "shortDescription" .=? reportingDescriptorShortDescription,
        "fullDescription" .= reportingDescriptorFullDescription,
        "messageStrings" .=? reportingDescriptorMessageStrings,
        "helpUri" .=? reportingDescriptorHelpUri,
        "help" .=? reportingDescriptorHelp,
        "defaultConfiguration" .=? reportingDescriptorDefaultConfiguration,
        "relationships" .=? reportingDescriptorRelationships,
        "properties" .=? reportingDescriptorProperties
      ]

instance FromJSON ReportingDescriptor where
  parseJSON = withObject "ReportingDescriptor" $ \obj ->
    MkReportingDescriptor
      <$> obj .: "id"
      <*> obj .:? "deprecatedIds"
      <*> obj .:? "guid"
      <*> obj .:? "deprecatedGuids"
      <*> obj .:? "name"
      <*> obj .:? "deprecatedNames"
      <*> obj .:? "shortDescription"
      <*> obj .:? "fullDescription"
      <*> obj .:? "messageStrings"
      <*> obj .:? "helpUri"
      <*> obj .:? "help"
      <*> obj .:? "defaultConfiguration"
      <*> obj .:? "relationships"
      <*> obj .:? "properties"
