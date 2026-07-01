--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Edge where

import Data.Aeson.Optional
import Data.Map
import Data.SARIF.Message
import Data.SARIF.Node
import Data.Text

data Edge = MkEdge
  { -- | The id property of the edge
    edgeId :: Text,
    -- | The label property of the edge
    edgeLabel :: Maybe Message,
    -- | The sourceNodeId property of the edge
    edgeSourceNodeId :: Node,
    -- | The targetNodeId property of the edge
    edgeTargetNodeId :: Node,
    -- | The properties property of the Edge object
    edgeProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Edge where
  toJSON MkEdge {..} =
    object
      [ "id" .= edgeId,
        "label" .=? edgeLabel,
        "sourceNodeId" .= edgeSourceNodeId,
        "targetNodeId" .= edgeTargetNodeId,
        "properties" .=? edgeProperties
      ]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \obj ->
    MkEdge
      <$> obj .: "id"
      <*> obj .:? "label"
      <*> obj .: "sourceNodeId"
      <*> obj .: "targetNodeId"
      <*> obj .:? "properties"
