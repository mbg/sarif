--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------
module Data.SARIF.WebResponse
  ( WebResponse (..),
  )
where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.ArtifactContent
import Data.Text

data WebResponse = MkWebResponse
  { -- | The index property of a web response object
    webResponseIndex :: Maybe Integer,
    -- | The protocal property of a web request object
    webResponseProtocol :: Text,
    -- | The version property of a web response object
    webResponseVersion :: Text,
    -- | The target property of a web response object
    webResponseStatusCode :: Integer,
    -- | The method property of a web response object
    webResponseReasonPhrase :: Text,
    -- | The headers property of a web response object
    webResponseHeaders :: Map Text Text,
    -- | The body property of a web response object
    webResponseBody :: Maybe ArtifactContent,
    -- | The noResponseReceived property of a web response object
    webResponseNoResponseReceived :: Maybe Bool,
    -- | The properties property of a WebResponse object
    webResponseProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON WebResponse where
  toJSON MkWebResponse {..} =
    object
      [ "index" .=? webResponseIndex,
        "protocol" .= webResponseProtocol,
        "version" .= webResponseVersion,
        "statusCode" .= webResponseStatusCode,
        "reasonPhrase" .= webResponseReasonPhrase,
        "headers" .= webResponseHeaders,
        "body" .=? webResponseBody,
        "noResponseReceived" .=? webResponseNoResponseReceived,
        "properties" .=? webResponseProperties
      ]

instance FromJSON WebResponse where
  parseJSON = withObject "WebResponse" $ \obj ->
    MkWebResponse
      <$> obj .:? "index"
      <*> obj .: "protocol"
      <*> obj .: "version"
      <*> obj .: "statusCode"
      <*> obj .: "reasonPhrase"
      <*> obj .: "headers"
      <*> obj .:? "body"
      <*> obj .:? "noResponseReceived"
      <*> obj .:? "properties"