--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

module Data.SARIF.Level
  ( Level (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson.Optional

-- | A `Level` value represents the severity of a result.
data Level
  = -- | The concept of “severity” does not apply.
    LevelNone
  | -- | A minor problem or an opportunity to improve the code was found.
    LevelNote
  | -- | A problem was found.
    LevelWarning
  | -- | A serious problem was found.
    LevelError
  deriving (Eq, Show, Ord)

instance ToJSON Level where
  toJSON LevelNone = "none"
  toJSON LevelNote = "note"
  toJSON LevelWarning = "warning"
  toJSON LevelError = "error"

instance FromJSON Level where
  parseJSON (String "none") = pure LevelNone
  parseJSON (String "note") = pure LevelNote
  parseJSON (String "warning") = pure LevelWarning
  parseJSON (String "error") = pure LevelError
  parseJSON _ = fail "Unexpected value for result level"

--------------------------------------------------------------------------------
