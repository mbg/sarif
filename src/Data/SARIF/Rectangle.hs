--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Rectangle where

import Data.Aeson.Optional
import Data.Map.Strict
import Data.SARIF.Message
import Data.Text

data Rectangle = MkRectangle
  { -- | The top property of the rectangle
    rectangleTop :: Int,
    -- | The left property of the rectangle
    rectangleLeft :: Int,
    -- | The bottom property of the rectangle
    rectangleBottom :: Int,
    -- | The right property of the rectangle
    rectangleRight :: Int,
    -- | The message property of the rectangle
    rectangleMessage :: Message,
    -- | The properties property of the Rectangle object
    rectangleProperties :: Maybe (Map Text Value)
  }
  deriving (Eq, Show, Ord)

instance ToJSON Rectangle where
  toJSON MkRectangle {..} =
    object
      [ "top" .= rectangleTop,
        "left" .= rectangleLeft,
        "bottom" .= rectangleBottom,
        "right" .= rectangleRight,
        "message" .= rectangleMessage,
        "properties" .=? rectangleProperties
      ]

instance FromJSON Rectangle where
  parseJSON = withObject "Rectangle" $ \obj ->
    MkRectangle
      <$> obj .: "top"
      <*> obj .: "left"
      <*> obj .: "bottom"
      <*> obj .: "right"
      <*> obj .: "message"
      <*> obj .:? "properties"