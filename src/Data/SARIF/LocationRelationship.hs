--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.LocationRelationship
  ( LocationRelationship (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.Text

--------------------------------------------------------------------------------

data LocationRelationshipKind
  = LocationRelIncludes
  | LocationRelIsIncludedBy
  | LocationRelRelevant
  | LocationRelCustom Text
  deriving (Eq, Show, Ord)

instance ToJSON LocationRelationshipKind where
  toJSON LocationRelIncludes = String "includes"
  toJSON LocationRelIsIncludedBy = String "isIncludedBy"
  toJSON LocationRelRelevant = String "relevant"
  toJSON (LocationRelCustom customKind) = String customKind

instance FromJSON LocationRelationshipKind where
  parseJSON = withText "LocationRelationshipKind" $ \case
    "includes" -> pure LocationRelIncludes
    "isIncludedBy" -> pure LocationRelIsIncludedBy
    "relevant" -> pure LocationRelRelevant
    t' -> pure $ LocationRelCustom t'

data LocationRelationship = MkLocationRelationship
  { -- | The target property of a LocationRelationship object
    logicalRelTarget :: Integer,
    -- | The name property of a LocationRelationship object
    logicalRelKinds :: Maybe [LocationRelationshipKind],
    -- | The description property of a LocationRelationship object
    logicalRelDescription :: Maybe Message,
    -- | The properties property of the LocationRelationship object
    logicalRelProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON LocationRelationship where
  toJSON MkLocationRelationship {..} =
    object
      [ "target" .= logicalRelTarget,
        "kinds" .= logicalRelKinds,
        "description" .=? logicalRelDescription,
        "properties" .=? logicalRelProperties
      ]

instance FromJSON LocationRelationship where
  parseJSON = withObject "LocationRelationship" $ \obj ->
    MkLocationRelationship
      <$> obj .: "target"
      <*> obj .:? "kinds"
      <*> obj .:? "description"
      <*> obj .:? "properties"
