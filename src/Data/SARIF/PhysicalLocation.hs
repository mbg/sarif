--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides the `ReportingDescriptor` type, which is essentially used to
-- represent descriptions of rules that a static analysis tool has applied.
module Data.SARIF.PhysicalLocation
  ( PhysicalLocation (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Address
import Data.SARIF.ArtifactLocation
import Data.SARIF.Region
import Data.Text

data PhysicalLocation = MkPhysicalLocation
  { -- | The artifactLocation property of a physicalLocation object
    physicalLocArtifactLocation :: Maybe ArtifactLocation,
    -- | The region property of a physicalLocation object
    physicalLocRegion :: Maybe Region,
    -- | The contextRegion property of a physicalLocation object
    physicalLocContextRegion :: Maybe Region,
    -- | The address property of a physicalLocation object
    physicalLocAddress :: Maybe Address,
    -- | The properties property of the PhysicalLocation object
    physicalLocProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON PhysicalLocation where
  toJSON MkPhysicalLocation {..} =
    object
      [ "artifactLocation" .=? physicalLocArtifactLocation,
        "region" .=? physicalLocRegion,
        "contextRegion" .=? physicalLocContextRegion,
        "address" .=? physicalLocAddress,
        "properties" .=? physicalLocProperties
      ]

instance FromJSON PhysicalLocation where
  parseJSON = withObject "PhysicalLocation" $ \obj ->
    MkPhysicalLocation
      <$> obj .:? "artifactLocation"
      <*> obj .:? "region"
      <*> obj .:? "contextRegion"
      <*> obj .:? "address"
      <*> obj .:? "properties"
