module Data.SARIF.ThreadFlowLocation (ThreadFlowLocation (..), ThreadFlowLocationKind (..), ThreadFlowImportance (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Location
import Data.SARIF.MultiformatMessageString
import Data.SARIF.ReportingDescriptorReference
import Data.SARIF.Stack
import Data.SARIF.WebRequest
import Data.SARIF.WebResponse
import Data.Text
import Data.Time

data ThreadFlowLocationKind
  = ThreadFlowLocationAcquireKind
  | ThreadFlowLocationReleaseKind
  | ThreadFlowLocationEnterKind
  | ThreadFlowLocationExitKind
  | ThreadFlowLocationCallKind
  | ThreadFlowLocationReturnKind
  | ThreadFlowLocationBranchKind
  | ThreadFlowLocationTaintKind
  | ThreadFlowLocationFunctionKind
  | ThreadFlowLocationHandlerKind
  | ThreadFlowLocationLockKind
  | ThreadFlowLocationMemoryKind
  | ThreadFlowLocationResourceKind
  | ThreadFlowLocationScopeKind
  | ThreadFlowLocationValueKind
  | ThreadFlowLocationImplicitKind
  | ThreadFlowLocationFalseKind
  | ThreadFlowLocationTrueKind
  | ThreadFlowLocationCautionKind
  | ThreadFlowLocationDangerKind
  | ThreadFlowLocationUnknownKind
  | ThreadFlowLocationUnreachableKind
  | ThreadFlowLocationCustomKind Text
  deriving (Eq, Show, Ord)

instance ToJSON ThreadFlowLocationKind where
  toJSON ThreadFlowLocationAcquireKind = String "acquire"
  toJSON ThreadFlowLocationReleaseKind = String "release"
  toJSON ThreadFlowLocationEnterKind = String "enter"
  toJSON ThreadFlowLocationExitKind = String "exixt"
  toJSON ThreadFlowLocationCallKind = String "call"
  toJSON ThreadFlowLocationReturnKind = String "return"
  toJSON ThreadFlowLocationBranchKind = String "branch"
  toJSON ThreadFlowLocationTaintKind = String "taint"
  toJSON ThreadFlowLocationFunctionKind = String "function"
  toJSON ThreadFlowLocationHandlerKind = String "handler"
  toJSON ThreadFlowLocationLockKind = String "lock"
  toJSON ThreadFlowLocationMemoryKind = String "memory"
  toJSON ThreadFlowLocationResourceKind = String "resource"
  toJSON ThreadFlowLocationScopeKind = String "scope"
  toJSON ThreadFlowLocationValueKind = String "value"
  toJSON ThreadFlowLocationImplicitKind = String "implicit"
  toJSON ThreadFlowLocationFalseKind = String "false"
  toJSON ThreadFlowLocationTrueKind = String "true"
  toJSON ThreadFlowLocationCautionKind = String "caution"
  toJSON ThreadFlowLocationDangerKind = String "danger"
  toJSON ThreadFlowLocationUnknownKind = String "unknown"
  toJSON ThreadFlowLocationUnreachableKind = String "unreachable"
  toJSON (ThreadFlowLocationCustomKind text) = String text

instance FromJSON ThreadFlowLocationKind where
  parseJSON = withText "ThreadFlowLocationKind" $ \case
    "acquire" -> pure ThreadFlowLocationAcquireKind
    "release" -> pure ThreadFlowLocationReleaseKind
    "enter" -> pure ThreadFlowLocationEnterKind
    "exit" -> pure ThreadFlowLocationExitKind
    "call" -> pure ThreadFlowLocationCallKind
    "return" -> pure ThreadFlowLocationReturnKind
    "branch" -> pure ThreadFlowLocationBranchKind
    "taint" -> pure ThreadFlowLocationTaintKind
    "function" -> pure ThreadFlowLocationFunctionKind
    "handler" -> pure ThreadFlowLocationHandlerKind
    "lock" -> pure ThreadFlowLocationLockKind
    "memory" -> pure ThreadFlowLocationMemoryKind
    "resource" -> pure ThreadFlowLocationResourceKind
    "scope" -> pure ThreadFlowLocationScopeKind
    "value" -> pure ThreadFlowLocationValueKind
    "implicit" -> pure ThreadFlowLocationImplicitKind
    "false" -> pure ThreadFlowLocationFalseKind
    "true" -> pure ThreadFlowLocationTrueKind
    "caution" -> pure ThreadFlowLocationCautionKind
    "danger" -> pure ThreadFlowLocationDangerKind
    "unknown" -> pure ThreadFlowLocationUnknownKind
    "unreachable" -> pure ThreadFlowLocationUnreachableKind
    custom -> pure $ ThreadFlowLocationCustomKind custom

data ThreadFlowImportance
  = ThreadFlowImportantImportance
  | ThreadFlowEssentialImportance
  | ThreadFlowUnimportantImportance
  deriving (Eq, Show, Ord)

instance ToJSON ThreadFlowImportance where
  toJSON ThreadFlowImportantImportance = String "important"
  toJSON ThreadFlowEssentialImportance = String "essential"
  toJSON ThreadFlowUnimportantImportance = String "unimportant"

instance FromJSON ThreadFlowImportance where
  parseJSON = withText "ThreadFlowImportance" $ \case
    "important" -> pure ThreadFlowImportantImportance
    "essential" -> pure ThreadFlowEssentialImportance
    "unimportant" -> pure ThreadFlowUnimportantImportance
    _ -> fail "Unknown ThreadFlowImportance"

data ThreadFlowLocation = MkThreadFlowLocation
  { -- | The index property of a thread flow location object
    threadFlowLocationIndex :: Maybe Integer,
    -- | The location property of a thread flow location object
    threadFlowLocationLocation :: Location,
    -- | The module property of a thread flow location object
    threadFlowLocationModule :: Maybe Text,
    -- | The stack property of a thread flow location object
    threadFlowLocationStack :: Maybe Stack,
    -- | The webRequest property of a thread flow location object
    threadFlowLocationWebRequest :: Maybe WebRequest,
    -- | The webResponse property of a thread flow location object
    threadFlowLocationWebResponse :: Maybe WebResponse,
    -- | The kinds property of a thread flow location object
    threadFlowLocationKinds :: Maybe [ThreadFlowLocationKind],
    -- | The state property of a thread flow location object
    threadFlowLocationState :: Maybe (Map Text MultiformatMessageString),
    -- | The nestingLevel property of a thread flow locatiob object
    threadFlowLocationNestingLevel :: Maybe Integer,
    -- | The executionOrder property of a thread flow location object
    threadFlowLocationExecutionOrder :: Maybe Integer,
    -- | The executionTimeUtc property of a thread flow location object
    threadFlowLocationExecutionTimeUtc :: Maybe UTCTime,
    -- | The importance property of a thread flow location object
    threadFlowLocationImportance :: Maybe Integer,
    -- | The taxa property of a thread flow location object
    threadFlowLocationTaxa :: Maybe [ReportingDescriptorReference],
    -- | The properties property of the ThreadFlowLocation object
    threadFlowLocationProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON ThreadFlowLocation where
  toJSON MkThreadFlowLocation {..} =
    object
      [ "index" .=? threadFlowLocationIndex,
        "location" .= threadFlowLocationLocation,
        "module" .=? threadFlowLocationModule,
        "stack" .=? threadFlowLocationStack,
        "webRequest" .=? threadFlowLocationWebRequest,
        "webResponse" .=? threadFlowLocationWebResponse,
        "kinds" .=? threadFlowLocationKinds,
        "state" .=? threadFlowLocationState,
        "nestingLevel" .=? threadFlowLocationNestingLevel,
        "executionOrder" .=? threadFlowLocationExecutionOrder,
        "executionTimeUtc" .=? threadFlowLocationExecutionTimeUtc,
        "importance" .=? threadFlowLocationImportance,
        "taxa" .=? threadFlowLocationTaxa,
        "properties" .=? threadFlowLocationProperties
      ]

instance FromJSON ThreadFlowLocation where
  parseJSON = withObject "ThreadFlowLocation" $ \obj ->
    MkThreadFlowLocation
      <$> obj .:? "index"
      <*> obj .: "location"
      <*> obj .:? "module"
      <*> obj .:? "stack"
      <*> obj .:? "webRequest"
      <*> obj .:? "webResponse"
      <*> obj .:? "kinds"
      <*> obj .:? "state"
      <*> obj .:? "nestingLevel"
      <*> obj .:? "executionOrder"
      <*> obj .:? "executionTimeUtc"
      <*> obj .:? "importance"
      <*> obj .:? "taxa"
      <*> obj .:? "properties"