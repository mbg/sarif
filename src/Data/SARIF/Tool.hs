--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Tool
  ( Tool (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.SARIF.ToolComponent

data Tool = MkTool
  { -- | The description of the tool itself.
    toolDriver :: ToolComponent,
    -- | Descriptions of any extensions to the tool that were used.
    toolExtensions :: Maybe [ToolComponent]
  }
  deriving (Eq, Show, Ord)

instance ToJSON Tool where
  toJSON MkTool {..} =
    object
      [ "driver" .= toolDriver,
        "extensions" .=? toolExtensions
      ]

instance FromJSON Tool where
  parseJSON = withObject "tool" $ \obj ->
    MkTool
      <$> obj .: "driver"
      <*> (obj .:? "extensions") .!= Just []

--------------------------------------------------------------------------------
