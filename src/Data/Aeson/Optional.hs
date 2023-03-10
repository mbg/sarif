--------------------------------------------------------------------------------
-- SARIF implementation for Haskell
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

-- | Provides extensions to "Data.Aeson" for constructing JSON objects which
-- omit fields that are @null@.
module Data.Aeson.Optional (
    module Aeson,
    object,
    (.=),
    (.=?)
) where

--------------------------------------------------------------------------------

import Data.Aeson as Aeson hiding (object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.Aeson as JSON (object)
import Data.Maybe
#if !MIN_VERSION_aeson(2,0,0)
import Data.Text
#endif

--------------------------------------------------------------------------------

-- | `object` @pairList@ constructs a JSON objects from @pairList@, omitting
-- all `Nothing` values.
object :: [Maybe Pair] -> Value
object = JSON.object . catMaybes

infixr 8 .=?, .=
-- | Construct an optional field from an optional value. If the value is
-- `Nothing`, then the field will be omitted from the JSON object.
#if MIN_VERSION_aeson(2,0,0)
(.=?) :: ToJSON a => Key -> Maybe a -> Maybe Pair
#else
(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
#endif
_ .=? Nothing = Nothing
key .=? Just val = Just (key, toJSON val)

-- | Construct a mandatory field from a value. The resulting field will always
-- be present in the resulting JSON object.
#if MIN_VERSION_aeson(2,0,0)
(.=) :: ToJSON a => Key -> a -> Maybe Pair
#else
(.=) :: ToJSON a => Text -> a -> Maybe Pair
#endif
key .= val = Just (key, toJSON val)

--------------------------------------------------------------------------------
