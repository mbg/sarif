--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.TranslationMetadata (TranslationMetadata (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.MultiformatMessageString
import Data.Text

data TranslationMetadata = MkTranslationMetadata
  { -- | the name property of the translation metadata
    translationMetadataName :: Text,
    -- | The fullName property of the translation metadata
    translationMetadataFullName :: Maybe Text,
    -- | The shortDescription property of the translation metadata
    translationMetadataShortDescription :: Maybe MultiformatMessageString,
    -- | The fullDescription property of the translation metadata
    translationMetadataFullDescription :: Maybe MultiformatMessageString,
    -- | The downloadUri property of the translation metadata
    translationMetadataDownloadUri :: Maybe Text,
    -- | The informationUri property of the translation metadata
    translationMetadataInformationUri :: Maybe Text,
    -- | The properties property of a TranslationMetadata object
    translationMetadataProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON TranslationMetadata where
  toJSON MkTranslationMetadata {..} =
    object
      [ "name" .= translationMetadataName,
        "fullName" .=? translationMetadataFullName,
        "shortDescription" .=? translationMetadataShortDescription,
        "fullDescription" .=? translationMetadataFullDescription,
        "downloadUri" .=? translationMetadataDownloadUri,
        "informationUri" .=? translationMetadataInformationUri,
        "properties" .=? translationMetadataProperties
      ]

instance FromJSON TranslationMetadata where
  parseJSON = withObject "TranslationMetadata" $ \obj ->
    MkTranslationMetadata
      <$> obj .: "name"
      <*> obj .:? "fullName"
      <*> obj .:? "shortDescription"
      <*> obj .:? "fullDescription"
      <*> obj .:? "downloadUri"
      <*> obj .:? "informationUri"
      <*> obj .:? "properties"
