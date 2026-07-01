--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Invocation (Invocation (..)) where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactLocation
import Data.SARIF.ConfigurationOverride
import Data.SARIF.Notification
import Data.Text

data Invocation = MkInvocation
  { -- | The commandLine property of the invocation
    invocationCommandLine :: Maybe Text,
    -- | The arguments property of the invocation
    invocationArguments :: Maybe [Text],
    -- | The responseFiles property of the invocation
    invocationResponseFiles :: Maybe [ArtifactLocation],
    -- | The ruleConfigurationOverrides property of the invocation
    invocationRuleConfigurationOverrides :: Maybe [ConfigurationOverride],
    -- | The notificationConfigurationOverrides property of the invocation
    invocationNotificationConfigurationOverrides :: Maybe [ConfigurationOverride],
    -- | The startTimeUtc property of the invocation
    invocationStartTimeUtc :: Maybe Text,
    -- | The endTimeUtc property of the invocation
    invocationEndTimeUtc :: Maybe Text,
    -- | The exitCode property of the invocation
    invocationExitCode :: Maybe Int,
    -- | The exitCodeDescription property of the invocation
    invocationExitCodeDescription :: Maybe Text,
    -- | The exitSignalName property of the invocation
    invocationExitSignalName :: Maybe Text,
    -- | The exitSignalNumber property of the invocation
    invocationExitSignalNumber :: Maybe Int,
    -- | The processStartFailureMessage property of the invocation
    invocationProcessStartFailureMessage :: Maybe Text,
    -- | The executionSuccessful property of the invocation
    invocationExecutionSuccessful :: Bool,
    -- | The machine property of the invocation
    invocationMachine :: Maybe Text,
    -- | The account property of the invocation
    invocationAccount :: Maybe Text,
    -- | The processId property of the invocation
    invocationProcessId :: Maybe Int,
    -- | The executableLocation property of the invocation
    invocationExecutableLocation :: Maybe ArtifactLocation,
    -- | The workingDirectory property of the invocation
    invocationWorkingDirectory :: Maybe ArtifactLocation,
    -- | The environmentVariables property of the invocation
    invocationEnvironmentVariables :: Maybe (Map Text Text),
    -- | The toolExecutionNotifications property of the invocation
    invocationToolExecutionNotifications :: Maybe [Notification],
    -- | The toolConfigurationNotifications property of the invocation
    invocationToolConfigurationNotifications :: Maybe [Notification],
    -- | The stdin property of the invocation
    invocationStdin :: Maybe ArtifactLocation,
    -- | The stdout property of the invocation
    invocationStdout :: Maybe ArtifactLocation,
    -- | The stderr property of the invocation
    invocationStderr :: Maybe ArtifactLocation,
    -- | The stdoutStderr property of the invocation
    invocationStdoutStderr :: Maybe ArtifactLocation,
    -- | The properties property of the Invocation object
    invocationProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Invocation where
  toJSON MkInvocation {..} =
    object
      [ "commandLine" .=? invocationCommandLine,
        "arguments" .=? invocationArguments,
        "responseFiles" .=? invocationResponseFiles,
        "ruleConfigurationOverrides" .=? invocationRuleConfigurationOverrides,
        "notificationConfigurationOverrides" .=? invocationNotificationConfigurationOverrides,
        "startTimeUtc" .=? invocationStartTimeUtc,
        "endTimeUtc" .=? invocationEndTimeUtc,
        "exitCode" .=? invocationExitCode,
        "exitCodeDescription" .=? invocationExitCodeDescription,
        "exitSignalName" .=? invocationExitSignalName,
        "exitSignalNumber" .=? invocationExitSignalNumber,
        "processStartFailureMessage" .=? invocationProcessStartFailureMessage,
        "executionSuccessful" .= invocationExecutionSuccessful,
        "machine" .=? invocationMachine,
        "account" .=? invocationAccount,
        "processId" .=? invocationProcessId,
        "executableLocation" .=? invocationExecutableLocation,
        "workingDirectory" .=? invocationWorkingDirectory,
        "environmentVariables" .=? invocationEnvironmentVariables,
        "toolExecutionNotifications" .=? invocationToolExecutionNotifications,
        "toolConfigurationNotifications" .=? invocationToolConfigurationNotifications,
        "stdin" .=? invocationStdin,
        "stdout" .=? invocationStdout,
        "stderr" .=? invocationStderr,
        "stdoutStderr" .=? invocationStdoutStderr,
        "properties" .=? invocationProperties
      ]

instance FromJSON Invocation where
  parseJSON = withObject "Invocation" $ \obj ->
    MkInvocation
      <$> obj .:? "commandLine"
      <*> obj .:? "arguments"
      <*> obj .:? "responseFiles"
      <*> obj .:? "ruleConfigurationOverrides"
      <*> obj .:? "notificationConfigurationOverrides"
      <*> obj .:? "startTimeUtc"
      <*> obj .:? "endTimeUtc"
      <*> obj .:? "exitCode"
      <*> obj .:? "exitCodeDescription"
      <*> obj .:? "exitSignalName"
      <*> obj .:? "exitSignalNumber"
      <*> obj .:? "processStartFailureMessage"
      <*> obj .: "executionSuccessful"
      <*> obj .:? "machine"
      <*> obj .:? "account"
      <*> obj .:? "processId"
      <*> obj .:? "executableLocation"
      <*> obj .:? "workingDirectory"
      <*> obj .:? "environmentVariables"
      <*> obj .:? "toolExecutionNotifications"
      <*> obj .:? "toolConfigurationNotifications"
      <*> obj .:? "stdin"
      <*> obj .:? "stdout"
      <*> obj .:? "stderr"
      <*> obj .:? "stdoutStderr"
      <*> obj .:? "properties"
