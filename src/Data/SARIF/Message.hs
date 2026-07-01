--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --

module Data.SARIF.Message
  ( Message (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.Text

data Message = MkMessage
  { -- | The text property of a message object
    messageText :: Maybe Text,
    -- | The markdown property of a message object
    messageMarkdown :: Maybe Text,
    -- | The id property of a message object
    messageId :: Maybe Text,
    -- | The arguments property of a message object
    messageArguments :: Maybe [Text],
    -- | The properties property of the Message object
    messageProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Message where
  toJSON MkMessage {..} =
    object
      [ "text" .=? messageText,
        "markdown" .=? messageMarkdown,
        "id" .=? messageId,
        "arguments" .=? messageArguments,
        "properties" .=? messageProperties
      ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \obj ->
    MkMessage
      <$> (obj .:? "text")
      <*> (obj .:? "markdown")
      <*> obj .:? "id"
      <*> obj .:? "arguments"
      <*> obj .:? "properties"
