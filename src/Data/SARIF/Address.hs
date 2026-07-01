--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SARIF.Address
  ( Address (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.Text

--------------------------------------------------------------------------------

data AddressKind
  = DataKind
  | HeaderKind
  | FunctionKind
  | InstructionKind
  | PageKind
  | SectionKind
  | SegmentKind
  | StackKind
  | ModuleKind
  | TableKind
  deriving (Eq, Show, Ord)

instance ToJSON AddressKind where
  toJSON DataKind = String "data"
  toJSON HeaderKind = String "header"
  toJSON FunctionKind = String "function"
  toJSON InstructionKind = String "instruction"
  toJSON PageKind = String "page"
  toJSON SectionKind = String "section"
  toJSON SegmentKind = String "segment"
  toJSON StackKind = String "stack"
  toJSON ModuleKind = String "module"
  toJSON TableKind = String "table"

instance FromJSON AddressKind where
  parseJSON = withText "AddressKind" $ \t ->
    case t of
      "data" -> pure DataKind
      "header" -> pure HeaderKind
      "function" -> pure FunctionKind
      "instruction" -> pure InstructionKind
      "page" -> pure PageKind
      "section" -> pure SectionKind
      "segment" -> pure SegmentKind
      "stack" -> pure StackKind
      "module" -> pure ModuleKind
      "table" -> pure TableKind
      _ -> fail $ "Unknown AddressKind: " ++ unpack t

--------------------------------------------------------------------------------

-- | Represents default configurations for `ReportingDescriptor` values. That is
-- properties which may be overriden by individual results.
data Address = MkAddress
  { -- | The index property of an Address object
    addressIndex :: Maybe Int,
    -- | The absoluteAddress property of an Address object
    addressAbsoluteAddress :: Maybe Integer,
    -- | The relativeAddress property of an Address object
    addressRelativeAddress :: Maybe Integer,
    -- | The offsetFromParent property of an Address object
    addressOffsetFromParent :: Maybe Integer,
    -- | The length property of an Address object
    addressLength :: Maybe Integer,
    -- | The name property of an Address object
    addressName :: Maybe Text,
    -- | The fullyQualifiedName property of an Address object
    addressFullyQualifiedName :: Maybe Text,
    -- | The kind property of an Address object
    addressKind :: Maybe AddressKind,
    -- | The parentIndex property of an Address object
    addressParentIndex :: Maybe Int,
    -- | The properties property of an Address object
    addressProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Address where
  toJSON MkAddress {..} =
    object
      [ "index" .=? addressIndex,
        "absoluteAddress" .=? addressAbsoluteAddress,
        "relativeAddress" .=? addressRelativeAddress,
        "offsetFromParent" .=? addressOffsetFromParent,
        "length" .=? addressLength,
        "name" .=? addressName,
        "fullyQualifiedName" .=? addressFullyQualifiedName,
        "kind" .=? addressKind,
        "parentIndex" .=? addressParentIndex
      ]

instance FromJSON Address where
  parseJSON = withObject "Address" $ \obj ->
    MkAddress
      <$> obj .:? "index"
      <*> (obj .:? "absoluteAddress")
      <*> obj .:? "relativeAddress"
      <*> obj .:? "offsetFromParent"
      <*> obj .:? "length"
      <*> obj .:? "name"
      <*> obj .:? "fullyQualifiedName"
      <*> obj .:? "kind"
      <*> obj .:? "parentIndex"
      <*> obj .:? "properties"
