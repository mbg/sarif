--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.CodeFlow where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ThreadFlow
import Data.Text

data CodeFlow = MkCodeFlow
  { codeFlowMessage :: Maybe Text,
    codeFlowThreadFlows :: [ThreadFlow],
    codeFlowProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON CodeFlow where
  toJSON MkCodeFlow {..} =
    object
      [ "message" .=? codeFlowMessage,
        "threadFlows" .= codeFlowThreadFlows,
        "properties" .=? codeFlowProperties
      ]

instance FromJSON CodeFlow where
  parseJSON = withObject "CodeFlow" $ \obj ->
    MkCodeFlow
      <$> obj .:? "message"
      <*> obj .: "threadFlows"
      <*> obj .:? "properties"
