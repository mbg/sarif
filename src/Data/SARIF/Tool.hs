--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Tool` type which is used to describe static analysis tools.
module Data.SARIF.Tool (
    Tool(..),
    ToolComponent(..),
    defaultToolComponent
) where

--------------------------------------------------------------------------------

import GHC.Generics

import Data.Aeson.Optional
import Data.Text
import Data.UUID.Types

import Data.SARIF.ReportingDescriptor

--------------------------------------------------------------------------------

-- | Used to describe static analysis tools.
data Tool = MkTool {
    -- | The description of the tool itself.
    toolDriver :: ToolComponent,
    -- | Descriptions of any extensions to the tool that were used.
    toolExtensions :: [ToolComponent]
} deriving (Eq, Show, Generic)

instance ToJSON Tool where
    toJSON MkTool{..} = object
        [ "driver" .= toolDriver
        , "extensions" .= toolExtensions
        ]

instance FromJSON Tool where
    parseJSON = withObject "Tool" $ \obj ->
        MkTool <$> obj .: "driver"
               <*> obj .: "extensions" .!= []

-- | A description of a tool component, such as a static analysis tool or
-- an extension to one.
data ToolComponent = MkToolComponent {
    -- | The short name of the tool component.
    toolComponentName :: Maybe Text,
    -- | The full name of the tool component.
    toolComponentFullName :: Maybe Text,
    -- | The semver of the tool component.
    toolComponentSemanticVersion :: Maybe Text,
    -- | The version of the tool component.
    toolComponentVersion :: Maybe Text,
    -- | A GUID which identifies the tool component.
    toolComponentGUID :: Maybe UUID,
    -- | A URI pointing to more information about the tool component.
    toolComponentInformationUri :: Maybe Text,
    -- | A list of rules that the tool component applies.
    toolComponentRules :: [ReportingDescriptor]
} deriving (Eq, Show, Generic)

instance ToJSON ToolComponent where
    toJSON MkToolComponent{..} = object
        [ "name" .= toolComponentName
        , "fullName" .=? toolComponentFullName
        , "semanticVersion" .=? toolComponentSemanticVersion
        , "version" .=? toolComponentVersion
        , "guid" .=? toolComponentGUID
        , "informationUri" .=? toolComponentInformationUri
        , "rules" .= toolComponentRules
        ]

instance FromJSON ToolComponent where
    parseJSON = withObject "ToolComponent" $ \obj ->
        MkToolComponent <$> obj .: "name"
                        <*> obj .: "fullName"
                        <*> obj .:? "semanticVersion"
                        <*> obj .:? "version"
                        <*> obj .:? "guid"
                        <*> obj .:? "informationUri"
                        <*> obj .: "rules" .!= []

-- | `defaultToolComponent` is a default value of the `ToolComponent` type.
defaultToolComponent :: ToolComponent
defaultToolComponent = MkToolComponent{
    toolComponentName = Nothing,
    toolComponentFullName = Nothing,
    toolComponentSemanticVersion = Nothing,
    toolComponentVersion = Nothing,
    toolComponentGUID = Nothing,
    toolComponentInformationUri = Nothing,
    toolComponentRules = []
}

--------------------------------------------------------------------------------
