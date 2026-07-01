--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ExternalProperties where

import Data.Aeson.Optional
import Data.SARIF.ExternalPropertyFileReferences
import Data.Text

data ExternalProperties = MkExternalProperties
  { -- | The schema property of the external properties
    externalPropertiesSchema :: Maybe Text,
    -- | The version property of the external properties
    externalPropertiesVersion :: Maybe Text,
    -- | The guid property of the external properties
    externalPropertiesGuid :: Text,
    -- | The runGuid property of the external properties
    externalPropertiesRunGuid :: Maybe Text,
    -- | The properties property of the external properties
    externalPropertiesProperties :: Maybe ExternalPropertyFileReferences
  }
  deriving (Eq, Show, Ord)

instance ToJSON ExternalProperties where
  toJSON MkExternalProperties {..} =
    object
      [ "$schema" .=? externalPropertiesSchema,
        "version" .=? externalPropertiesVersion,
        "guid" .= externalPropertiesGuid,
        "runGuid" .=? externalPropertiesRunGuid,
        "properties" .=? externalPropertiesProperties
      ]

instance FromJSON ExternalProperties where
  parseJSON = withObject "ExternalProperties" $ \obj ->
    MkExternalProperties
      <$> obj .:? "$schema"
      <*> obj .:? "version"
      <*> obj .: "guid"
      <*> obj .:? "runGuid"
      <*> obj .:? "properties"
