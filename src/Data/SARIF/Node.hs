--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Node where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Location
import Data.SARIF.Message
import Data.Text

data Node = MkNode
  { -- | The id property of the node
    nodeId :: Text,
    -- | The message property of the node
    nodeMessage :: Maybe Message,
    -- | The location property of the node
    nodeLocation :: Location,
    -- | The children property of the node
    nodeChildren :: Maybe [Node],
    -- | The properties property of the Node object
    nodeProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Node where
  toJSON MkNode {..} =
    object
      [ "id" .= nodeId,
        "message" .=? nodeMessage,
        "location" .= nodeLocation,
        "children" .=? nodeChildren,
        "properties" .=? nodeProperties
      ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \obj ->
    MkNode
      <$> obj .: "id"
      <*> obj .:? "message"
      <*> obj .: "location"
      <*> obj .:? "children"
      <*> obj .:? "properties"
