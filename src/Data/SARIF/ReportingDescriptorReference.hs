--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.ReportingDescriptorReference
  ( ReportingDescriptorReference (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ToolComponentReference
import Data.Text
import Data.UUID.Types

data ReportingDescriptorReference = MkReportingDescriptorReference
  { -- | An identifier which should equals to the `id` property of a reportingDescriptor
    reportingDescriptorReferenceId :: Maybe Text,
    -- | An index into the `toolComponent.rules` array
    reportingDescriptorReferenceIndex :: Maybe Integer,
    -- | A GUID which identifies the ReportingDescriptor.
    reportingDescriptorReferenceGuid :: Maybe UUID,
    -- | A reference to a `ToolComponent`.
    reportingDescriptorReferenceToolComponent :: Maybe ToolComponentReference,
    -- | The properties property of the ReportingDescriptorReference object
    reportingDescriptorReferenceProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ReportingDescriptorReference where
  toJSON MkReportingDescriptorReference {..} =
    object
      [ "id" .=? reportingDescriptorReferenceId,
        "index" .=? reportingDescriptorReferenceIndex,
        "guid" .=? reportingDescriptorReferenceGuid,
        "toolComponent" .=? reportingDescriptorReferenceToolComponent,
        "properties" .=? reportingDescriptorReferenceProperties
      ]

instance FromJSON ReportingDescriptorReference where
  parseJSON = withObject "ReportingDescriptorReference" $ \obj ->
    MkReportingDescriptorReference
      <$> (obj .: "id")
      <*> (obj .:? "index")
      <*> obj .:? "guid"
      <*> obj .:? "toolComponent"
      <*> obj .:? "properties"
