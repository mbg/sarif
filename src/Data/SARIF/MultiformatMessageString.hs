--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.MultiformatMessageString
  ( MultiformatMessageString (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.Text

data MultiformatMessageString = MkMultiformatMessageString
  { -- | A textual representation of the message, which is mandatory.
    mmsText :: Text,
    -- | Optionally, a markdown representation of the message.
    mmsMarkdown :: Maybe Text,
    -- | The properties property of the MultiformatMessageString object
    mmsProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON MultiformatMessageString where
  toJSON MkMultiformatMessageString {..} =
    object
      [ "text" .= mmsText,
        "markdown" .=? mmsMarkdown,
        "properties" .=? mmsProperties
      ]

instance FromJSON MultiformatMessageString where
  parseJSON = withObject "MultiformatMessageString" $ \obj ->
    MkMultiformatMessageString
      <$> obj .: "text"
      <*> obj .:? "markdown"
      <*> obj .:? "properties"
