--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Attachment where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Location
import Data.SARIF.Message
import Data.SARIF.Rectangle
import Data.SARIF.Region
import Data.Text

data Attachment = MkAttachment
  { -- | The description property of the attachment
    attachmentDescription :: Maybe Message,
    -- | The location property of the attachment
    attachmentLocation :: Maybe Location,
    -- | The region property of the attachment
    attachmentRegion :: Maybe [Region],
    -- | The rectangle property of the attachment
    attachmentRectangle :: Maybe [Rectangle],
    -- | The properties property of an Attachment object
    attachmentProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Attachment where
  toJSON MkAttachment {..} =
    object
      [ "description" .=? attachmentDescription,
        "location" .=? attachmentLocation,
        "region" .=? attachmentRegion,
        "rectangle" .=? attachmentRectangle,
        "properties" .=? attachmentProperties
      ]

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \obj ->
    MkAttachment
      <$> obj .:? "description"
      <*> obj .:? "location"
      <*> obj .:? "region"
      <*> obj .:? "rectangle"
      <*> obj .:? "properties"
