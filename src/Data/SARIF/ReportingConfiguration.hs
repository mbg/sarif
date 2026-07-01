--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.ReportingConfiguration
  ( ReportingConfiguration (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import qualified Data.Map.Lazy as M
import Data.SARIF.Level
import Data.Text

data ReportingConfiguration = MkReportingConfiguration
  { -- | Flag indicating whether theDescriptor was checked for during the scan.
    reportingConfigurationEnabled :: Bool,
    -- | The default severity of the reporting descriptor.
    reportingConfigurationLevel :: Maybe Level,
    -- | A float representing the priority or importance of the result.
    reportingConfigurationRank :: Maybe Double,
    -- | Configuration information that is specific to that descriptor
    reportingConfigurationParameters :: Maybe (M.Map Text Value),
    -- | The properties property of the ReportingConfiguration object
    reportingConfigurationProperties :: Maybe (M.Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ReportingConfiguration where
  toJSON MkReportingConfiguration {..} =
    object
      [ "enabled" .= reportingConfigurationEnabled,
        "level" .=? reportingConfigurationLevel,
        "rank" .=? reportingConfigurationRank,
        "parameters" .=? reportingConfigurationParameters,
        "properties" .=? reportingConfigurationProperties
      ]

instance FromJSON ReportingConfiguration where
  parseJSON = withObject "ReportingConfiguration" $ \obj ->
    MkReportingConfiguration
      <$> (obj .: "enabled")
      <*> (obj .:? "level")
      <*> obj .:? "rank"
      <*> obj .:? "parameters"
      <*> obj .:? "properties"
