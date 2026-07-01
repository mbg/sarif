--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Run
  ( Run (..),
    RunColumnKind (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional hiding (Result)
import Data.Map.Strict
import Data.SARIF.Address
import Data.SARIF.Artifact
import Data.SARIF.ArtifactLocation
import Data.SARIF.AutomationDetails
import Data.SARIF.Conversion
import Data.SARIF.ExternalPropertyFileReferences
import Data.SARIF.Graph
import Data.SARIF.Invocation
import Data.SARIF.LogicalLocation
import Data.SARIF.Result
import Data.SARIF.SpecialLocations
import Data.SARIF.ThreadFlowLocation
import Data.SARIF.Tool
import Data.SARIF.ToolComponent
import Data.SARIF.VersionControlDetails
import Data.SARIF.WebRequest
import Data.SARIF.WebResponse
import Data.Text

--------------------------------------------------------------------------------

data RunColumnKind
  = RunUtf16CodeUnitsColumnKind
  | RunUnicodeCodePointsColumnKind
  | RunCustomColumnKind Text
  deriving (Eq, Show, Ord)

instance ToJSON RunColumnKind where
  toJSON RunUtf16CodeUnitsColumnKind = String "utf16CodeUnits"
  toJSON RunUnicodeCodePointsColumnKind = String "unicodeCodePoints"
  toJSON (RunCustomColumnKind t) = String t

instance FromJSON RunColumnKind where
  parseJSON = withText "RunColumnKind" $ \case
    "utf16CodeUnits" -> pure RunUtf16CodeUnitsColumnKind
    "unicodeCodePoints" -> pure RunUnicodeCodePointsColumnKind
    other -> pure $ RunCustomColumnKind other

-- | Represents individual runs of static analysis tools.
data Run = MkRun
  { -- | The externalPropertyFileReferences property of a run
    runExternalPropertyFileReferences :: Maybe ExternalPropertyFileReferences,
    -- | The automationDetails property of a run
    runAutomationDetails :: Maybe AutomationDetails,
    -- | The runAggregates property of a run
    runAggregates :: Maybe [AutomationDetails],
    -- | The baselineGuid property of a run
    runBaselineGuid :: Maybe Text,
    -- | The tool property of a run
    runTool :: Maybe Tool,
    -- | The language property of a run
    runLanguage :: Maybe Text,
    -- | The taxonomies property of a run
    runTaxonomies :: Maybe [ToolComponent],
    -- | The translations property of a run
    runTranslations :: Maybe [ToolComponent],
    -- | The policies property of a run
    runPolicies :: Maybe [ToolComponent],
    -- | The invocations property of a run
    runInvocations :: Maybe [Invocation],
    -- | The conversion property of a run
    runConversion :: Maybe Conversion,
    -- | The versionControlProvenance property of a run
    runVersionControlProvenance :: Maybe VersionControlDetails,
    -- | The originalUriBaseIds property of a run
    runOriginalUriBaseIds :: Maybe (Map Text ArtifactLocation),
    -- | The artifacts property of a run
    runArtifacts :: Maybe [Artifact],
    -- | The specialLocations property of a run
    runSpecialLocations :: Maybe [SpecialLocations],
    -- | The logicalLocations property of a run
    runLogicalLocations :: Maybe [LogicalLocation],
    -- | The addresses property of a run
    runAddresses :: Maybe [Address],
    -- | The threadFlowLocations property of a run
    runThreadFlowLocations :: Maybe [ThreadFlowLocation],
    -- | The graphs property of a run
    runGraphs :: Maybe [Graph],
    -- | The webRequests property of a run
    runWebRequests :: Maybe [WebRequest],
    -- | The webResponses property of a run
    runWebResponses :: Maybe [WebResponse],
    -- | The results property of a run
    runResults :: Maybe [Result],
    -- | The defaultEncoding property of a run
    runDefaultEncoding :: Maybe Text,
    -- | The defaultSourceLanguage property of a run
    runDefaultSourceLanguage :: Maybe Text,
    -- | The newlineSequences property of a run
    runNewlineSequences :: Maybe [Text],
    -- | The columnKind property of a run
    runColumnKind :: Maybe RunColumnKind,
    -- | The redactionTokens property of a run
    runRedactionTokens :: Maybe [Text],
    -- | The properties property of the Run object
    runProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------

instance ToJSON Run where
  toJSON MkRun {..} =
    object
      [ "externalPropertyFileReferences" .=? runExternalPropertyFileReferences,
        "automationDetails" .=? runAutomationDetails,
        "runAggregates" .=? runAggregates,
        "baselineGuid" .=? runBaselineGuid,
        "tool" .=? runTool,
        "language" .=? runLanguage,
        "taxonomies" .=? runTaxonomies,
        "translations" .=? runTranslations,
        "policies" .=? runPolicies,
        "invocations" .=? runInvocations,
        "conversion" .=? runConversion,
        "versionControlProvenance" .=? runVersionControlProvenance,
        "originalUriBaseIds" .=? runOriginalUriBaseIds,
        "artifacts" .=? runArtifacts,
        "specialLocations" .=? runSpecialLocations,
        "logicalLocations" .=? runLogicalLocations,
        "addresses" .=? runAddresses,
        "threadFlowLocations" .=? runThreadFlowLocations,
        "graphs" .=? runGraphs,
        "webRequests" .=? runWebRequests,
        "webResponses" .=? runWebResponses,
        "results" .=? runResults,
        "defaultEncoding" .=? runDefaultEncoding,
        "defaultSourceLanguage" .=? runDefaultSourceLanguage,
        "newlineSequences" .=? runNewlineSequences,
        "columnKind" .=? runColumnKind,
        "redactionTokens" .=? runRedactionTokens,
        "properties" .=? runProperties
      ]

instance FromJSON Run where
  parseJSON = withObject "Run" $ \obj ->
    MkRun
      <$> obj .:? "externalPropertyFileReferences"
      <*> obj .:? "automationDetails"
      <*> obj .:? "runAggregates"
      <*> obj .:? "baselineGuid"
      <*> obj .:? "tool"
      <*> obj .:? "language"
      <*> obj .:? "taxonomies"
      <*> obj .:? "translations"
      <*> obj .:? "policies"
      <*> obj .:? "invocations"
      <*> obj .:? "conversion"
      <*> obj .:? "versionControlProvenance"
      <*> obj .:? "originalUriBaseIds"
      <*> obj .:? "artifacts"
      <*> obj .:? "specialLocations"
      <*> obj .:? "logicalLocations"
      <*> obj .:? "addresses"
      <*> obj .:? "threadFlowLocations"
      <*> obj .:? "graphs"
      <*> obj .:? "webRequests"
      <*> obj .:? "webResponses"
      <*> obj .:? "results"
      <*> obj .:? "defaultEncoding"
      <*> obj .:? "defaultSourceLanguage"
      <*> obj .:? "newlineSequences"
      <*> obj .:? "columnKind"
      <*> obj .:? "redactionTokens"
      <*> obj .:? "properties"
