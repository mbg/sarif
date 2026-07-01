--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ReportingDescriptorRelationship where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.SARIF.ReportingDescriptorReference
import Data.Text

data ReportingDescriptorRelationshipKind
  = ReportingDescriptorRelationshipEqualKind
  | ReportingDescriptorRelationshipSupersetKind
  | ReportingDescriptorRelationshipSubsetKind
  | ReportingDescriptorRelationshipDisjointKind
  | ReportingDescriptorRelationshipIncomparableKind
  | ReportingDescriptorRelationshipCanFollowKind
  | ReportingDescriptorRelationshipCanPrecedeKind
  | ReportingDescriptorRelationshipWillFollowKind
  | ReportingDescriptorRelationshipWillPrecedeKind
  | ReportingDescriptorRelationshipRelevantKind
  | ReportingDescriptorRelationshipCustomKind Text
  deriving (Eq, Show, Ord)

instance ToJSON ReportingDescriptorRelationshipKind where
  toJSON ReportingDescriptorRelationshipEqualKind = String "equal"
  toJSON ReportingDescriptorRelationshipSupersetKind = String "superset"
  toJSON ReportingDescriptorRelationshipSubsetKind = String "subset"
  toJSON ReportingDescriptorRelationshipDisjointKind = String "disjoint"
  toJSON ReportingDescriptorRelationshipIncomparableKind = String "incomparable"
  toJSON ReportingDescriptorRelationshipCanFollowKind = String "canFollow"
  toJSON ReportingDescriptorRelationshipCanPrecedeKind = String "canPrecede"
  toJSON ReportingDescriptorRelationshipWillFollowKind = String "willFollow"
  toJSON ReportingDescriptorRelationshipWillPrecedeKind = String "willPrecede"
  toJSON ReportingDescriptorRelationshipRelevantKind = String "relevant"
  toJSON (ReportingDescriptorRelationshipCustomKind t) = String t

instance FromJSON ReportingDescriptorRelationshipKind where
  parseJSON = withText "ReportingDescriptorRelationshipKind" $ \t ->
    case t of
      "equal" -> pure ReportingDescriptorRelationshipEqualKind
      "superset" -> pure ReportingDescriptorRelationshipSupersetKind
      "subset" -> pure ReportingDescriptorRelationshipSubsetKind
      "disjoint" -> pure ReportingDescriptorRelationshipDisjointKind
      "incomparable" -> pure ReportingDescriptorRelationshipIncomparableKind
      "canFollow" -> pure ReportingDescriptorRelationshipCanFollowKind
      "canPrecede" -> pure ReportingDescriptorRelationshipCanPrecedeKind
      "willFollow" -> pure ReportingDescriptorRelationshipWillFollowKind
      "willPrecede" -> pure ReportingDescriptorRelationshipWillPrecedeKind
      "relevant" -> pure ReportingDescriptorRelationshipRelevantKind
      _ -> pure (ReportingDescriptorRelationshipCustomKind t)

data ReportingDescriptorRelationship = MkReportingDescriptorRelationship
  { -- | The target property of the reporting descriptor relationship
    reportingDescriptorRelationshipTarget :: ReportingDescriptorReference,
    -- | The kinds property of the reporting descriptor relationship
    reportingDescriptorRelationshipKinds :: [ReportingDescriptorRelationshipKind],
    -- | The description property of the reporting descriptor relationship
    reportingDescriptorRelationshipDescription :: Maybe Message,
    -- | The properties property of the ReportingDescriptorRelationship object
    reportingDescriptorRelationshipProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ReportingDescriptorRelationship where
  toJSON MkReportingDescriptorRelationship {..} =
    object
      [ "target" .= reportingDescriptorRelationshipTarget,
        "kinds" .= reportingDescriptorRelationshipKinds,
        "description" .=? reportingDescriptorRelationshipDescription,
        "properties" .=? reportingDescriptorRelationshipProperties
      ]

instance FromJSON ReportingDescriptorRelationship where
  parseJSON = withObject "ReportingDescriptorRelationship" $ \obj ->
    MkReportingDescriptorRelationship
      <$> obj .: "target"
      <*> obj .: "kinds"
      <*> obj .:? "description"
      <*> obj .:? "properties"