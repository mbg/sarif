--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.StackFrame (StackFrame (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Location
import Data.Text

data StackFrame = MkStackFrame
  { -- | The location property of a stack frame object
    stackFrameLocation :: Maybe Location,
    -- | The module property of a stack frame object
    stackFrameModule :: Maybe Text,
    -- | The threadId property of a stack frame object
    stackFrameThreadId :: Maybe Integer,
    -- | The parameters property of a stack frame object
    stackFrameParameters :: Maybe [Text],
    -- | The properties property of the StackFrame object
    stackFrameProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON StackFrame where
  toJSON MkStackFrame {..} =
    object
      [ "location" .=? stackFrameLocation,
        "module" .=? stackFrameModule,
        "threadId" .=? stackFrameThreadId,
        "parameters" .=? stackFrameParameters,
        "properties" .=? stackFrameProperties
      ]

instance FromJSON StackFrame where
  parseJSON = withObject "StackFrame" $ \obj ->
    MkStackFrame
      <$> obj .:? "location"
      <*> obj .:? "module"
      <*> obj .:? "threadId"
      <*> obj .:? "parameters"
      <*> obj .:? "properties"
