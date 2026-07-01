--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ThreadFlow (ThreadFlow (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.SARIF.ThreadFlowLocation
import Data.Text

data ThreadFlow = MkThreadFlow
  { -- | The id property of a thread flow object
    threadFlowId :: Maybe Text,
    -- | The message property of a thread flow object
    threadFlowMessage :: Maybe Message,
    -- | The  initialState property of a thread flow object
    threadFlowInitialState :: Maybe (Map Text Text),
    -- | The immutableState property of a thread flow object
    threadFlowImmutableState :: Maybe (Map Text Text),
    -- | The locations property of a thread flow object
    threadFlowLocations :: [ThreadFlowLocation],
    -- | The properties property of the ThreadFlow object
    threadFlowProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ThreadFlow where
  toJSON MkThreadFlow {..} =
    object
      [ "id" .=? threadFlowId,
        "message" .=? threadFlowMessage,
        "initialState" .=? threadFlowInitialState,
        "immutableState" .=? threadFlowImmutableState,
        "locations" .= threadFlowLocations,
        "properties" .=? threadFlowProperties
      ]

instance FromJSON ThreadFlow where
  parseJSON = withObject "ThreadFlow" $ \obj ->
    MkThreadFlow
      <$> obj .:? "id"
      <*> obj .:? "message"
      <*> obj .:? "initialState"
      <*> obj .:? "immutableState"
      <*> obj .: "locations"
      <*> obj .:? "properties"
