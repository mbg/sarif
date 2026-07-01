--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Stack (Stack (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.SARIF.StackFrame
import Data.Text

data Stack = MkStack
  { -- | The message property of a thread flow object
    stackMessage :: Maybe Message,
    -- | The frames property of a thread flow object
    stackFrames :: [StackFrame],
    -- | The properties property of the Stack object
    stackProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Stack where
  toJSON MkStack {..} =
    object
      [ "message" .=? stackMessage,
        "frames" .= stackFrames,
        "properties" .=? stackProperties
      ]

instance FromJSON Stack where
  parseJSON = withObject "Stack" $ \obj ->
    MkStack
      <$> obj .:? "message"
      <*> obj .: "frames"
      <*> obj .:? "properties"
