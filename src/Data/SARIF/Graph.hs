--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Graph (Graph (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Edge
import Data.SARIF.Message
import Data.SARIF.Node
import Data.Text

data Graph = MkGraph
  { -- | The description property of the graph
    graphDescription :: Maybe Message,
    -- | The nodes property of the graph,
    graphNodes :: Maybe [Node],
    -- | The edges property of the graph
    graphEdges :: Maybe [Edge],
    -- | The properties property of the Graph object
    graphProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Graph where
  toJSON MkGraph {..} =
    object
      [ "description" .=? graphDescription,
        "nodes" .=? graphNodes,
        "edges" .=? graphEdges,
        "properties" .=? graphProperties
      ]

instance FromJSON Graph where
  parseJSON = withObject "Graph" $ \obj ->
    MkGraph
      <$> obj .:? "description"
      <*> obj .:? "nodes"
      <*> obj .:? "edges"
      <*> obj .:? "properties"
