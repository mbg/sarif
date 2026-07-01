{-# LANGUAGE InstanceSigs #-}

--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.ToolComponentReference
  ( ToolComponentReference (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.Text
import Data.UUID.Types

data ToolComponentReference = MkToolComponentReference
  { -- | A name which should equals to the `name` property of a toolComponent
    toolComponentReferenceName :: Maybe Text,
    -- | An index into the `extensions` array
    toolComponentReferenceIndex :: Maybe Text,
    -- | A GUID  which should equals to the `guid` property of a toolComponent
    toolComponentReferenceGuid :: Maybe UUID,
    -- | The properties property of a ToolComponentReference object
    toolComponentReferenceProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ToolComponentReference where
  toJSON MkToolComponentReference {..} =
    object
      [ "name" .=? toolComponentReferenceName,
        "index" .=? toolComponentReferenceIndex,
        "guid" .=? toolComponentReferenceGuid,
        "properties" .=? toolComponentReferenceProperties
      ]

instance FromJSON ToolComponentReference where
  parseJSON = withObject "ToolComponentReference" $ \obj ->
    MkToolComponentReference
      <$> (obj .:? "name")
      <*> (obj .:? "index")
      <*> obj .:? "guid"
      <*> obj .:? "properties"
