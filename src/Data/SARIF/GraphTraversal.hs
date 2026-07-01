--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.GraphTraversal where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.EdgeTraversal
import Data.SARIF.Message
import Data.SARIF.MultiformatMessageString
import Data.Text

data GraphTraversal = MkGraphTraversal
  { -- | The resultGraphIndex property of the graph traversal
    graphTraversalResultGraphIndex :: Maybe Int,
    -- | The runGraphIndex property of the graph traversal
    graphTraversalRunGraphIndex :: Maybe Int,
    -- | The description property of the graph traversal
    graphTraversalDescription :: Maybe Message,
    -- | The initialState property of the graph traversal
    graphTraversalInitialState :: Map Text MultiformatMessageString,
    -- | The immutableState property of the graph traversal
    graphTraversalImmutableState :: Map Text MultiformatMessageString,
    -- | The edgeTraversals property of the graph traversal
    graphTraversalEdgeTraversals :: Maybe [EdgeTraversal],
    -- | The properties property of the GraphTraversal object
    graphTraversalProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON GraphTraversal where
  toJSON MkGraphTraversal {..} =
    object
      [ "resultGraphIndex" .=? graphTraversalResultGraphIndex,
        "runGraphIndex" .=? graphTraversalRunGraphIndex,
        "description" .=? graphTraversalDescription,
        "initialState" .= graphTraversalInitialState,
        "immutableState" .= graphTraversalImmutableState,
        "edgeTraversals" .=? graphTraversalEdgeTraversals,
        "properties" .=? graphTraversalProperties
      ]

instance FromJSON GraphTraversal where
  parseJSON = withObject "GraphTraversal" $ \obj ->
    MkGraphTraversal
      <$> obj .:? "resultGraphIndex"
      <*> obj .:? "runGraphIndex"
      <*> obj .:? "description"
      <*> obj .: "initialState"
      <*> obj .: "immutableState"
      <*> obj .:? "edgeTraversals"
      <*> obj .:? "properties"
