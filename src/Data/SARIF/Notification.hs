--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Notification where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Exception
import Data.SARIF.Location
import Data.SARIF.Message
import Data.SARIF.ReportingDescriptorReference
import Data.Text

data NotificationLevel
  = NotificationErrorLevel
  | NotificationWarningLevel
  | NotificationNoteLevel
  | NotificationNoneLevel
  deriving (Eq, Show, Ord)

instance ToJSON NotificationLevel where
  toJSON NotificationErrorLevel = String "error"
  toJSON NotificationWarningLevel = String "warning"
  toJSON NotificationNoteLevel = String "note"
  toJSON NotificationNoneLevel = String "none"

instance FromJSON NotificationLevel where
  parseJSON = withText "NotificationLevel" $ \case
    "error" -> pure NotificationErrorLevel
    "warning" -> pure NotificationWarningLevel
    "note" -> pure NotificationNoteLevel
    "none" -> pure NotificationNoneLevel
    _ -> fail "Unknown NotificationLevel"

data Notification = MkNotification
  { -- | The descriptor property of the notification
    notificationDescriptor :: ReportingDescriptorReference,
    -- | The associatedRule property of the notification
    notificationAssociatedRule :: Maybe ReportingDescriptorReference,
    -- | The locations property of the notification
    notificationLocations :: Maybe [Location],
    -- | The message property of the notification
    notificationMessage :: Maybe Message,
    -- | The level property of the notification
    notificationLevel :: Maybe NotificationLevel,
    -- | The threadId property of the notification
    notificationThreadId :: Maybe Int,
    -- | The timeUtc property of the notification
    notificationTimeUtc :: Maybe Text,
    -- | The exception property of the notification
    notificationException :: Maybe Exception,
    -- | The properties property of the Notification object
    notificationProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Notification where
  toJSON MkNotification {..} =
    object
      [ "descriptor" .= notificationDescriptor,
        "associatedRule" .=? notificationAssociatedRule,
        "locations" .=? notificationLocations,
        "message" .=? notificationMessage,
        "level" .=? notificationLevel,
        "threadId" .=? notificationThreadId,
        "timeUtc" .=? notificationTimeUtc,
        "exception" .=? notificationException,
        "properties" .=? notificationProperties
      ]

instance FromJSON Notification where
  parseJSON = withObject "Notification" $ \obj ->
    MkNotification
      <$> obj .: "descriptor"
      <*> obj .:? "associatedRule"
      <*> obj .:? "locations"
      <*> obj .:? "message"
      <*> obj .:? "level"
      <*> obj .:? "threadId"
      <*> obj .:? "timeUtc"
      <*> obj .:? "exception"
      <*> obj .:? "properties"
