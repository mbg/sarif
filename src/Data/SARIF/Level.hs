--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Provides the `Level` type, which enumerates SARIF result levels.
-- See https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html#_Ref493511208
module Data.SARIF.Level (
    Level(..)
) where

--------------------------------------------------------------------------------

import Data.Aeson.Optional hiding (Error)

--------------------------------------------------------------------------------

-- | A `Level` value represents the severity of a result.
data Level
    -- | The concept of “severity” does not apply.
    = None
    -- | A minor problem or an opportunity to improve the code was found.
    | Note
    -- | A problem was found.
    | Warning
    -- | A serious problem was found.
    | Error
    deriving (Eq, Show)

instance ToJSON Level where
    toJSON None = "none"
    toJSON Note = "note"
    toJSON Warning = "warning"
    toJSON Error = "error"

instance FromJSON Level where
    parseJSON (String "none") = pure None
    parseJSON (String "note") = pure Note
    parseJSON (String "warning") = pure Warning
    parseJSON (String "error") = pure Error
    parseJSON _ = fail "Unexpected value for result level"

--------------------------------------------------------------------------------
