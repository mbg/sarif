--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SARIF.LogicalLocation
  ( LogicalLocation (..),
  )
where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.Text

--------------------------------------------------------------------------------

data LogicalLocationKind
  = ExecutableCodeFunctionKind
  | ExecutableCodeMemberKind
  | ExecutableCodeModuleKind
  | ExecutableCodeNamespaceKind
  | ExecutableCodeResourceKind
  | ExecutableCodeTypeKind
  | ExecutableCodeParameterKind
  | ExecutableCodeVariableKind
  | XMLHTMLElementKind
  | XMLHTMLAttributeKind
  | XMLHTMLTextKind
  | XMLHTMLCommentKind
  | XMLHTMLProcessingInstructionKind
  | XMLHTMLDtdKind
  | XMLHTMLDeclarationKind
  | JSONObjectKind
  | JSONArrayKind
  | JSONPropertyKind
  | JSONValueKind
  deriving (Eq, Show, Ord)

instance ToJSON LogicalLocationKind where
  toJSON ExecutableCodeFunctionKind = String "function"
  toJSON ExecutableCodeMemberKind = String "member"
  toJSON ExecutableCodeModuleKind = String "module"
  toJSON ExecutableCodeNamespaceKind = String "namespace"
  toJSON ExecutableCodeResourceKind = String "resource"
  toJSON ExecutableCodeTypeKind = String "type"
  toJSON ExecutableCodeParameterKind = String "parameter"
  toJSON ExecutableCodeVariableKind = String "variable"
  toJSON XMLHTMLElementKind = String "element"
  toJSON XMLHTMLAttributeKind = String "attribute"
  toJSON XMLHTMLTextKind = String "text"
  toJSON XMLHTMLCommentKind = String "comment"
  toJSON XMLHTMLProcessingInstructionKind = String "processingInstruction"
  toJSON XMLHTMLDtdKind = String "dtd"
  toJSON XMLHTMLDeclarationKind = String "declaration"
  toJSON JSONObjectKind = String "object"
  toJSON JSONArrayKind = String "array"
  toJSON JSONPropertyKind = String "property"
  toJSON JSONValueKind = String "value"

instance FromJSON LogicalLocationKind where
  parseJSON = withText "LogicalLocationKind" $ \t ->
    case t of
      "function" -> pure ExecutableCodeFunctionKind
      "member" -> pure ExecutableCodeMemberKind
      "module" -> pure ExecutableCodeModuleKind
      "namespace" -> pure ExecutableCodeNamespaceKind
      "resource" -> pure ExecutableCodeResourceKind
      "type" -> pure ExecutableCodeTypeKind
      "parameter" -> pure ExecutableCodeParameterKind
      "variable" -> pure ExecutableCodeVariableKind
      "element" -> pure XMLHTMLElementKind
      "attribute" -> pure XMLHTMLAttributeKind
      "text" -> pure XMLHTMLTextKind
      "comment" -> pure XMLHTMLCommentKind
      "processingInstruction" -> pure XMLHTMLProcessingInstructionKind
      "dtd" -> pure XMLHTMLDtdKind
      "declaration" -> pure XMLHTMLDeclarationKind
      "object" -> pure JSONObjectKind
      "array" -> pure JSONArrayKind
      "property" -> pure JSONPropertyKind
      "value" -> pure JSONValueKind
      _ -> fail $ "Unknown LogicalLocationKind: " ++ unpack t

data LogicalLocation = MkLogicalLocation
  { -- | The index property of a logicalLocation object
    logicalLocIndex :: Maybe Integer,
    -- | The name property of a logicalLocation object
    logicalLocName :: Text,
    -- | The fullyQualifiedName property of a logicalLocation object
    logicalLocFullyQualifiedName :: Maybe Text,
    -- | The decoratedName property of a logicalLocation object
    logicalLocDecoratedName :: Maybe Text,
    -- | The kind property of a logicalLocation object
    logicalLocKind :: LogicalLocationKind,
    -- | The parentIndex property of a logicalLocation object
    logicalLocParentIndex :: Maybe Integer,
    -- | The properties property of the LogicalLocation object
    logicalLocProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON LogicalLocation where
  toJSON MkLogicalLocation {..} =
    object
      [ "index" .=? logicalLocIndex,
        "name" .= logicalLocName,
        "fullyQualifiedName" .=? logicalLocFullyQualifiedName,
        "decoratedName" .=? logicalLocDecoratedName,
        "kind" .= logicalLocKind,
        "parentIndex" .=? logicalLocParentIndex,
        "properties" .=? logicalLocProperties
      ]

instance FromJSON LogicalLocation where
  parseJSON = withObject "LogicalLocation" $ \obj ->
    MkLogicalLocation
      <$> obj .:? "index"
      <*> obj .: "name"
      <*> obj .:? "fullyQualifiedName"
      <*> obj .:? "decoratedName"
      <*> obj .: "kind"
      <*> obj .:? "parentIndex"
      <*> obj .:? "properties"
