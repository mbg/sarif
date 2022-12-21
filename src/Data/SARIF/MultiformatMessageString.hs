--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `MultiformatMessageString` type which is used to represent
-- messages in different formats, such as text or markdown.
module Data.SARIF.MultiformatMessageString (
    MultiformatMessageString(..),
    defaultMultiformatMessageString
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Text

--------------------------------------------------------------------------------

-- | Represents a message in at least textual representation, but also allowing
-- it to be provided in other formats such as markdown.
data MultiformatMessageString = MkMultiformatMessageString {
    -- | A textual representation of the message, which is mandatory.
    mmsText :: Text,
    -- | Optionally, a markdown representation of the message.
    mmsMarkdown :: Maybe Text
} deriving (Eq, Show)

instance ToJSON MultiformatMessageString where
    toJSON MkMultiformatMessageString{..} = object
        [ "text" .= mmsText
        , "markdown" .=? mmsMarkdown
        ]

instance FromJSON MultiformatMessageString where
    parseJSON = withObject "MultiformatMessageString" $ \obj ->
        MkMultiformatMessageString <$> obj .: "text"
                                   <*> obj .:? "markdown"

-- | `defaultMultiformatMessageString` @messageText@ constructs a
-- `MultiformatMessageString` value where @messageText@ is the textual
-- representation of the message.
defaultMultiformatMessageString :: Text -> MultiformatMessageString
defaultMultiformatMessageString text = MkMultiformatMessageString{
    mmsText = text,
    mmsMarkdown = Nothing
}

--------------------------------------------------------------------------------
