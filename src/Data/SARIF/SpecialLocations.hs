--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.SpecialLocations (SpecialLocations (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.Text

data SpecialLocations = MkSpecialLocations
  { -- | The displayBase property of the special locations
    specialLocationsDisplayBase :: Maybe ArtifactLocation,
    -- | The properties property of the SpecialLocations object
    specialLocationsProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON SpecialLocations where
  toJSON MkSpecialLocations {..} =
    object
      [ "displayBase" .=? specialLocationsDisplayBase,
        "properties" .=? specialLocationsProperties
      ]

instance FromJSON SpecialLocations where
  parseJSON = withObject "SpecialLocations" $ \obj ->
    MkSpecialLocations
      <$> obj .:? "displayBase"
      <*> obj .:? "properties"
