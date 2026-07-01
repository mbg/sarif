--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.AutomationDetails (AutomationDetails (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.Text

data AutomationDetails = MkAutomationDetails
  { -- | The description property of the automation details
    automationDetailsDescription :: Maybe Message,
    -- | The id property of the automation details
    automationDetailsId :: Maybe Text,
    -- | The guid property of the automation details
    automationDetailsGuid :: Maybe Text,
    -- | The correlationGuid property of the automation details
    automationDetailsCorrelationGuid :: Maybe Text,
    -- | The properties property of the automation details
    automationDetailsProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON AutomationDetails where
  toJSON MkAutomationDetails {..} =
    object
      [ "description" .=? automationDetailsDescription,
        "id" .=? automationDetailsId,
        "guid" .=? automationDetailsGuid,
        "correlationGuid" .=? automationDetailsCorrelationGuid,
        "properties" .=? automationDetailsProperties
      ]

instance FromJSON AutomationDetails where
  parseJSON = withObject "AutomationDetails" $ \obj ->
    MkAutomationDetails
      <$> obj .:? "description"
      <*> obj .:? "id"
      <*> obj .:? "guid"
      <*> obj .:? "correlationGuid"
      <*> obj .:? "properties"
