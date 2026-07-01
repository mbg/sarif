--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.EdgeTraversal where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Edge
import Data.SARIF.Message
import Data.SARIF.MultiformatMessageString
import Data.Text

data EdgeTraversal = MkEdgeTraversal
  { -- | The edgeId property of the edge traversal
    edgeTraversalEdgeId :: Edge,
    -- | The message property of the edge traversal
    edgeTraversalMessage :: Maybe Message,
    -- | The finalState property of the edge traversal
    edgeTraversalFinalState :: Maybe (Map Text MultiformatMessageString),
    -- | The stepOverEdgeCount property of the edge traversal
    edgeTraversalStepOverEdgeCount :: Maybe Int,
    -- | The properties property of the EdgeTraversal object
    edgeTraversalProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON EdgeTraversal where
  toJSON MkEdgeTraversal {..} =
    object
      [ "edgeId" .= edgeTraversalEdgeId,
        "message" .=? edgeTraversalMessage,
        "finalState" .=? edgeTraversalFinalState,
        "stepOverEdgeCount" .=? edgeTraversalStepOverEdgeCount,
        "properties" .=? edgeTraversalProperties
      ]

instance FromJSON EdgeTraversal where
  parseJSON = withObject "EdgeTraversal" $ \obj ->
    MkEdgeTraversal
      <$> obj .: "edgeId"
      <*> obj .:? "message"
      <*> obj .:? "finalState"
      <*> obj .:? "stepOverEdgeCount"
      <*> obj .:? "properties"