--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Location
  ( Location (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
-- import Data.SARIF.LocationRelationship

import Data.Map.Strict
import Data.SARIF.LocationRelationship
import Data.SARIF.LogicalLocation
import Data.SARIF.Message
import Data.SARIF.PhysicalLocation
import Data.SARIF.Region
import Data.Text

data Location = MkLocation
  { -- | The id property of a location object
    locationId :: Maybe Integer,
    -- | The physicalLocation property of a location object
    locationPhysicalLocation :: Maybe PhysicalLocation,
    -- | The logicalLocations property of a location object
    locationLogicalLocations :: Maybe [LogicalLocation],
    -- | The message property of a location object
    locationMessage :: Maybe Message,
    -- | The annotations property of a location object
    locationAnnotations :: Maybe [Region],
    -- | The relationship property of a location object
    locationRelationship :: Maybe [LocationRelationship],
    -- | The properties property of the Location object
    locationProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Location where
  toJSON MkLocation {..} =
    object
      [ "id" .=? locationId,
        "physicalLocation" .=? locationPhysicalLocation,
        "logicalLocation" .=? locationLogicalLocations,
        "message" .=? locationMessage,
        "annotations" .=? locationAnnotations,
        "relationships" .=? locationRelationship,
        "properties" .=? locationProperties
      ]

instance FromJSON Location where
  parseJSON = withObject "Location" $ \obj ->
    MkLocation
      <$> obj .:? "id"
      <*> obj .:? "physicalLocation"
      <*> obj .:? "logicalLocations"
      <*> obj .:? "message"
      <*> obj .:? "annotations"
      <*> obj .:? "relationships"
      <*> obj .:? "properties"
