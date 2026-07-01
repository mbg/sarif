--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.Suppression (Suppression (..), SuppressionKind (..), SuppressionStatus (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Location
import Data.Text
import Data.UUID.Types

data SuppressionKind
  = SuppressionInSourceKind
  | SuppressionExternalKind
  deriving (Eq, Show, Ord)

instance ToJSON SuppressionKind where
  toJSON SuppressionInSourceKind = String "inSource"
  toJSON SuppressionExternalKind = String "external"

instance FromJSON SuppressionKind where
  parseJSON = withText "SuppressionKind" $ \case
    "inSource" -> pure SuppressionInSourceKind
    "external" -> pure SuppressionExternalKind
    _ -> fail "Unknown SuppressionKind"

data SuppressionStatus
  = SuppressionAcceptedStatus
  | SuppressionRejectedStatus
  | SuppressionUnderReviewStatus
  deriving (Eq, Show, Ord)

instance ToJSON SuppressionStatus where
  toJSON SuppressionAcceptedStatus = String "accepted"
  toJSON SuppressionRejectedStatus = String "rejected"
  toJSON SuppressionUnderReviewStatus = String "underReview"

instance FromJSON SuppressionStatus where
  parseJSON = withText "SuppressionStatus" $ \case
    "accepted" -> pure SuppressionAcceptedStatus
    "rejected" -> pure SuppressionRejectedStatus
    "underReview" -> pure SuppressionUnderReviewStatus
    _ -> fail "Unknown SuppressionStatus"

data Suppression = MkSuppression
  { suppressionKind :: SuppressionKind,
    suppressionStatus :: Maybe SuppressionStatus,
    suppressionLocation :: Maybe Location,
    suppressionGUID :: Maybe UUID,
    suppressionJustification :: Maybe Text,
    suppressionProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Suppression where
  toJSON MkSuppression {..} =
    object
      [ "kind" .= suppressionKind,
        "status" .=? suppressionStatus,
        "location" .=? suppressionLocation,
        "guid" .=? suppressionGUID,
        "justification" .=? suppressionJustification,
        "properties" .=? suppressionProperties
      ]

instance FromJSON Suppression where
  parseJSON = withObject "Suppression" $ \o ->
    MkSuppression
      <$> o .: "kind"
      <*> o .:? "status"
      <*> o .:? "location"
      <*> o .:? "guid"
      <*> o .:? "justification"
      <*> o .:? "properties"
