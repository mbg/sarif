--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.WebRequest
  ( WebRequest (..),
  )
where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactContent
import Data.Text

data WebRequest = MkWebRequest
  { -- | The index property of a web request object
    webRequestIndex :: Maybe Integer,
    -- | The protocal property of a web request object
    webRequestProtocol :: Text,
    -- | The version property of a web request object
    webRequestVersion :: Text,
    -- | The target property of a web request object
    webRequestTarget :: Text,
    -- | The method property of a web request object
    webRequestMethod :: Text,
    -- | The headers property of a web request object
    webRequestHeaders :: Map Text Text,
    -- | The parameters property of a web request object
    webRequestParameters :: Maybe (Map Text Text),
    -- | The body property of a web request object
    webRequestBody :: Maybe ArtifactContent,
    -- | The properties property of a WebRequest object
    webRequestProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON WebRequest where
  toJSON MkWebRequest {..} =
    object
      [ "index" .=? webRequestIndex,
        "protocol" .= webRequestProtocol,
        "version" .= webRequestVersion,
        "target" .= webRequestTarget,
        "method" .= webRequestMethod,
        "headers" .= webRequestHeaders,
        "parameters" .=? webRequestParameters,
        "body" .=? webRequestBody,
        "properties" .=? webRequestProperties
      ]

instance FromJSON WebRequest where
  parseJSON = withObject "WebRequest" $ \obj ->
    MkWebRequest
      <$> obj .:? "index"
      <*> obj .: "protocol"
      <*> obj .: "version"
      <*> obj .: "target"
      <*> obj .: "method"
      <*> obj .: "headers"
      <*> obj .:? "parameters"
      <*> obj .:? "body"
      <*> obj .:? "properties"