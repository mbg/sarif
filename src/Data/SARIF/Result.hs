--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Result
  ( ResultKind (..),
    Result (..),
    ResultLevel (..),
    ResultBaselineState (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional hiding (Error, Result)
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.SARIF.Attachment
import Data.SARIF.CodeFlow
import Data.SARIF.Fix
import Data.SARIF.Graph
import Data.SARIF.GraphTraversal
import Data.SARIF.Location
import Data.SARIF.Message
import Data.SARIF.ReportingDescriptorReference (ReportingDescriptorReference)
import Data.SARIF.ResultProvenance
import Data.SARIF.Stack
import Data.SARIF.Suppression
import Data.SARIF.WebRequest
import Data.SARIF.WebResponse
import Data.Text
import Data.UUID.Types

--------------------------------------------------------------------------------

data ResultKind
  = ResultPassKind
  | ResultOpenKind
  | ResultInformationalKind
  | ResultNoApplicableKind
  | ResultReviewKind
  | ResultFailKind
  deriving (Eq, Show, Ord)

instance ToJSON ResultKind where
  toJSON ResultPassKind = String "pass"
  toJSON ResultOpenKind = String "open"
  toJSON ResultInformationalKind = String "informational"
  toJSON ResultNoApplicableKind = String "noApplicable"
  toJSON ResultReviewKind = String "review"
  toJSON ResultFailKind = String "fail"

instance FromJSON ResultKind where
  parseJSON = withText "ResultKind" $ \case
    "pass" -> pure ResultPassKind
    "open" -> pure ResultOpenKind
    "informational" -> pure ResultInformationalKind
    "noApplicable" -> pure ResultNoApplicableKind
    "review" -> pure ResultReviewKind
    "fail" -> pure ResultFailKind
    _ -> fail "Unknown ResultKind"

data ResultLevel
  = ResultWarningLevel
  | ResultErrorLevel
  | ResultNoteLevel
  | ResultNoneLevel
  deriving (Eq, Show, Ord)

instance ToJSON ResultLevel where
  toJSON ResultWarningLevel = String "warning"
  toJSON ResultErrorLevel = String "error"
  toJSON ResultNoteLevel = String "note"
  toJSON ResultNoneLevel = String "none"

instance FromJSON ResultLevel where
  parseJSON = withText "ResultLevel" $ \case
    "warning" -> pure ResultWarningLevel
    "error" -> pure ResultErrorLevel
    "note" -> pure ResultNoteLevel
    "none" -> pure ResultNoneLevel
    _ -> fail "Unknown Result Level"

data ResultBaselineState
  = ResultBaselineNewState
  | ResultBaselineUnchangedState
  | ResultBaselineUpdatedState
  | ResultBaselineAbsentState
  deriving (Eq, Show, Ord)

instance ToJSON ResultBaselineState where
  toJSON ResultBaselineNewState = String "new"
  toJSON ResultBaselineUnchangedState = String "unchanged"
  toJSON ResultBaselineUpdatedState = String "updated"
  toJSON ResultBaselineAbsentState = String "absent"

instance FromJSON ResultBaselineState where
  parseJSON = withText "ResultBaselineState" $ \case
    "new" -> pure ResultBaselineNewState
    "unchanged" -> pure ResultBaselineUnchangedState
    "updated" -> pure ResultBaselineUpdatedState
    "absent" -> pure ResultBaselineAbsentState
    _ -> fail "Unknown ResultBaselineState"

-- | Represents the results of a run of a static analysis tool.
data Result = MkResult
  { -- | A unique,stable and opaque identifier for the result.
    resultGuid :: Maybe UUID,
    -- | The identifier of a logically identical result
    resultCorrelationGuid :: Maybe UUID,
    -- | The unique ID of the rule of which this result is an occurrence of.
    resultRuleId :: Text,
    -- | A reference to the `ReportingDescriptor` for the rule
    resultRuleIndex :: Maybe Int,
    -- | The rule descriptor reference that identifies the report descriptor
    resultRule :: Maybe ReportingDescriptorReference,
    -- | A list of references to taxonomies to which this result belongs.
    resultTaxa :: Maybe [ReportingDescriptorReference],
    -- | The kind property of the result.
    resultKind :: Maybe ResultKind,
    -- | The level property of the result
    resultLevel :: Maybe ResultLevel,
    -- | A result-specific message which may refer to specific variable names
    -- etc. which caused the rule to trigger.
    resultMessage :: Message,
    -- | The locations property of the result.
    resultLocations :: [Location],
    -- | The analysisTarget property of the result
    resultAnalysisTarget :: Maybe ArtifactLocation,
    -- | The webRequest property of the result
    resultWebRequest :: Maybe WebRequest,
    -- | The webResponse property of the result
    resultWebResponse :: Maybe WebResponse,
    -- | The fingerprints property of the result
    resultFingerprints :: Maybe (Map Text Text),
    -- | The partialFingerprints property of the result
    resultPartialFingerprints :: Maybe (Map Text Text),
    -- | The codeFlows property of the result
    resultCodeFlows :: Maybe [CodeFlow],
    -- | The graph property of the result
    resultGraphs :: Maybe [Graph],
    -- | The graph traversal property of the result
    resultGraphTraversals :: Maybe [GraphTraversal],
    -- | The stacks property of the result
    resultStacks :: Maybe [Stack],
    -- | The relatedLocations property of the result
    resultRelatedLocations :: Maybe [Location],
    -- | The suppressions property of the result
    resultSuppressions :: Maybe [Suppression],
    -- | The baselineState property of the result
    resultBaselineState :: Maybe ResultBaselineState,
    -- | The rank property of the result
    resultRank :: Maybe Double,
    -- | The attachments property of the result
    resultAttachments :: Maybe [Attachment],
    -- | The workItemUris property of the result
    resultWorkItemUris :: Maybe [Text],
    -- | The hostedViewerUri property of the result
    resultHostedViewerUri :: Maybe Text,
    -- | The provenance property of the result
    resultProvenance :: Maybe ResultProvenance,
    -- | The fixes property of the result
    resultFixes :: Maybe [Fix],
    -- | The occurrenceCount property of the result
    resultOccurrenceCount :: Maybe Int,
    -- | The properties property of the Result object
    resultProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Result where
  toJSON MkResult {..} =
    object
      [ "guid" .=? resultGuid,
        "correlationGuid" .=? resultCorrelationGuid,
        "ruleId" .= resultRuleId,
        "ruleIndex" .=? resultRuleIndex,
        "rule" .=? resultRule,
        "taxa" .=? resultTaxa,
        "kind" .=? resultKind,
        "level" .=? resultLevel,
        "message" .= resultMessage,
        "locations" .= resultLocations,
        "analysisTarget" .=? resultAnalysisTarget,
        "webRequest" .=? resultWebRequest,
        "webResponse" .=? resultWebResponse,
        "fingerprints" .=? resultFingerprints,
        "partialFingerprints" .=? resultPartialFingerprints,
        "codeFlows" .=? resultCodeFlows,
        "graphs" .=? resultGraphs,
        "graphTraversals" .=? resultGraphTraversals,
        "stacks" .=? resultStacks,
        "relatedLocations" .=? resultRelatedLocations,
        "suppressions" .=? resultSuppressions,
        "baselineState" .=? resultBaselineState,
        "rank" .=? resultRank,
        "attachments" .=? resultAttachments,
        "workItemUris" .=? resultWorkItemUris,
        "hostedViewerUri" .=? resultHostedViewerUri,
        "provenance" .=? resultProvenance,
        "fixes" .=? resultFixes,
        "occurrenceCount" .=? resultOccurrenceCount,
        "properties" .=? resultProperties
      ]

instance FromJSON Result where
  parseJSON = withObject "Result" $ \obj ->
    MkResult
      <$> obj .:? "guid"
      <*> obj .:? "correlationGuid"
      <*> obj .: "ruleId"
      <*> obj .:? "ruleIndex"
      <*> obj .:? "rule"
      <*> obj .:? "taxa"
      <*> obj .:? "kind"
      <*> obj .:? "level"
      <*> obj .: "message"
      <*> obj .: "locations"
      <*> obj .:? "analysisTarget"
      <*> obj .:? "webRequest"
      <*> obj .:? "webResponse"
      <*> obj .:? "fingerprints"
      <*> obj .:? "partialFingerprints"
      <*> obj .:? "codeFlows"
      <*> obj .:? "graphs"
      <*> obj .:? "graphTraversals"
      <*> obj .:? "stacks"
      <*> obj .:? "relatedLocations"
      <*> obj .:? "suppressions"
      <*> obj .:? "baselineState"
      <*> obj .:? "rank"
      <*> obj .:? "attachments"
      <*> obj .:? "workItemUris"
      <*> obj .:? "hostedViewerUri"
      <*> obj .:? "provenance"
      <*> obj .:? "fixes"
      <*> obj .:? "occurrenceCount"
      <*> obj .:? "properties"

--------------------------------------------------------------------------------
