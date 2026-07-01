--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.ConfigurationOverride where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ReportingConfiguration
import Data.SARIF.ReportingDescriptorReference
import Data.Text

data ConfigurationOverride = MkConfigurationOverride
  { -- | The descriptor property of the configuration override
    configurationOverrideDescriptor :: ReportingDescriptorReference,
    -- | The configuration property of the configuration override
    configurationOverrideConfiguration :: ReportingConfiguration,
    -- | The properties property of the ConfigurationOverride object
    configurationOverrideProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ConfigurationOverride where
  toJSON MkConfigurationOverride {..} =
    object
      [ "descriptor" .= configurationOverrideDescriptor,
        "configuration" .= configurationOverrideConfiguration,
        "properties" .=? configurationOverrideProperties
      ]

instance FromJSON ConfigurationOverride where
  parseJSON = withObject "ConfigurationOverride" $ \obj ->
    MkConfigurationOverride
      <$> obj .: "descriptor"
      <*> obj .: "configuration"
      <*> obj .:? "properties"
