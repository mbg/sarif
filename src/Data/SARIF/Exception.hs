--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Exception (Exception (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Stack
import Data.Text

data Exception = MkException
  { -- | The kind property of the exception
    exceptionKind :: Text,
    -- | The message property of the exception
    exceptionMessage :: Text,
    -- | The stack property of the exception
    exceptionStack :: Maybe Stack,
    -- | The innerExceptions property of the exception
    exceptionInnerExceptions :: Maybe [Exception],
    -- | The properties property of the Exception object
    exceptionProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Exception where
  toJSON MkException {..} =
    object
      [ "kind" .= exceptionKind,
        "message" .= exceptionMessage,
        "stack" .=? exceptionStack,
        "innerExceptions" .=? exceptionInnerExceptions,
        "properties" .=? exceptionProperties
      ]

instance FromJSON Exception where
  parseJSON = withObject "Exception" $ \obj ->
    MkException
      <$> obj .: "kind"
      <*> obj .: "message"
      <*> obj .:? "stack"
      <*> obj .:? "innerExceptions"
      <*> obj .:? "properties"
