--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Run` type which represents a single run of a
-- static analysis tool.
module Data.SARIF.Run (
    Run(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson hiding (Result)

import Data.SARIF.Artifact
import Data.SARIF.Result (Result(..))
import Data.SARIF.Tool (Tool(..))

--------------------------------------------------------------------------------

-- | Represents individual runs of static analysis tools.
data Run = MkRun {
    -- | A description of the tool that was run.
    runTool :: Tool,
    -- | A list of the artifacts that were scanned by the tool.
    runArtifacts :: [Artifact],
    -- | The results produced by the tool as a result of scanning
    -- the artifacts.
    runResults :: [Result]
} deriving (Eq, Show)

instance ToJSON Run where
    toJSON MkRun{..} = object
        [ "tool" .= runTool
        , "artifacts" .= runArtifacts
        , "results" .= runResults
        ]

instance FromJSON Run where
    parseJSON = withObject "Run" $ \obj ->
        MkRun <$> obj .: "tool"
              <*> obj .: "artifacts" .!= []
              <*> obj .: "results" .!= []

--------------------------------------------------------------------------------
