--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ExternalPropertyFileReference where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.Text

data ExternalPropertyFileReference = MkExternalPropertyFileReference
  { -- | The location property of the external property file reference
    externalPropertyFileReferenceLocation :: Maybe ArtifactLocation,
    -- | The guid property of the external property file reference
    externalPropertyFileReferenceGuid :: Maybe Text,
    -- | The itemCount property of the external property file reference
    externalPropertyFileReferenceItemCount :: Maybe Int,
    -- | The properties property of the ExternalPropertyFileReference object
    externalPropertyFileReferenceProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ExternalPropertyFileReference where
  toJSON MkExternalPropertyFileReference {..} =
    object
      [ "location" .=? externalPropertyFileReferenceLocation,
        "guid" .=? externalPropertyFileReferenceGuid,
        "itemCount" .=? externalPropertyFileReferenceItemCount,
        "properties" .=? externalPropertyFileReferenceProperties
      ]

instance FromJSON ExternalPropertyFileReference where
  parseJSON = withObject "ExternalPropertyFileReference" $ \obj ->
    MkExternalPropertyFileReference
      <$> obj .:? "location"
      <*> obj .:? "guid"
      <*> obj .:? "itemCount"
      <*> obj .:? "properties"
