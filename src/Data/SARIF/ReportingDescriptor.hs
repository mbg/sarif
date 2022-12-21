--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `ReportingDescriptor` type, which is essentially used to
-- represent descriptions of rules that a static analysis tool has applied.
module Data.SARIF.ReportingDescriptor (
    ReportingConfiguration(..),
    defaultReportingConfiguration,
    ReportingDescriptor(..),
    defaultReportingDescriptor
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import qualified Data.Map.Lazy as M
import Data.Text

import Data.SARIF.Level
import Data.SARIF.MultiformatMessageString

--------------------------------------------------------------------------------

-- | Represents default configurations for `ReportingDescriptor` values. That is
-- properties which may be overriden by individual results.
newtype ReportingConfiguration = MkReportingConfiguration {
    -- | The default severity of the reporting descriptor.
    rcLevel :: Maybe Level
} deriving (Eq, Show)

instance ToJSON ReportingConfiguration where
    toJSON MkReportingConfiguration{..} = object
        [ "level" .=? rcLevel
        ]
instance FromJSON ReportingConfiguration where
    parseJSON = withObject "ReportingConfiguration" $ \obj ->
        MkReportingConfiguration <$> obj .: "level"

-- | `defaultReportingConfiguration` is a default
-- `ReportingConfiguration` value.
defaultReportingConfiguration :: ReportingConfiguration
defaultReportingConfiguration = MkReportingConfiguration{
    rcLevel = Nothing
}

-- | Represents rules that a static analysis tool
data ReportingDescriptor = MkReportingDescriptor {
    -- | The unique ID of the rule.
    rdId :: Text,
    -- | A friendly name for the rule, which should be one word in
    -- upper camel case.
    rdName :: Maybe Text,
    -- | A short description for the rule, which may contain spaces
    -- and symbols.
    rdShortDescription :: Maybe MultiformatMessageString,
    -- | The full description.
    rdFullDescription :: Maybe MultiformatMessageString,
    -- | A URL to some help for this rule.
    rdHelpUri :: Maybe Text,
    -- | A recommendation for what to do in order to resolve an occurrence
    -- of this rule.
    rdHelp :: Maybe MultiformatMessageString,
    -- | The default reporting configuration for the rule.
    rdDefaultConfiguration :: Maybe ReportingConfiguration,
    -- | Extra properties for this rule.
    rdProperties :: M.Map Text Value
} deriving (Eq, Show)

instance ToJSON ReportingDescriptor where
    toJSON MkReportingDescriptor{..} = object
        [ "id" .= rdId
        , "name" .=? rdName
        , "shortDescription" .=? rdShortDescription
        , "fullDescription" .=? rdFullDescription
        , "helpUri" .=? rdHelpUri
        , "help" .=? rdHelp
        , "defaultConfiguration" .=? rdDefaultConfiguration
        , "properties" .= rdProperties
        ]

instance FromJSON ReportingDescriptor where
    parseJSON = withObject "ReportingDescriptor" $ \obj ->
        MkReportingDescriptor <$> obj .: "id"
                              <*> obj .:? "name"
                              <*> obj .:? "shortDescription"
                              <*> obj .:? "fullDescription"
                              <*> obj .:? "helpUri"
                              <*> obj .:? "help"
                              <*> obj .:? "defaultConfiguration"
                              <*> obj .: "properties" .!= M.empty

-- | `defaultReportingDescriptor` @id@ constructs a default
-- `ReportingDescriptor` with the given @id@, which must be unique.
defaultReportingDescriptor :: Text -> ReportingDescriptor
defaultReportingDescriptor ident = MkReportingDescriptor{
    rdId = ident,
    rdName = Nothing,
    rdShortDescription = Nothing,
    rdFullDescription = Nothing,
    rdHelpUri = Nothing,
    rdHelp = Nothing,
    rdDefaultConfiguration = Nothing,
    rdProperties = M.empty
}

--------------------------------------------------------------------------------
