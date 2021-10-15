{-|
Module      : $Header$
Description : Types and conversions
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Text.Mustache.Types
  (
  -- * Types for the Parser / Template
    ASTree
  , STree
  , Node(..)
  , DataIdentifier(..)
  , Template(..)
  , TemplateCache
  -- * Types for the Substitution / Data
  , Value(..)
  , Key
  -- ** Converting
  , object
  , (~>), (↝), (~=), (⥱)
  , ToMustache, toMustache, mFromJSON, integralToMustache
  -- ** Representation
  , Array, Object, Pair
  , SubM, askContext, askPartials
  , Context(..)
  ) where


import           Control.Monad.Reader
import qualified Data.Aeson                   as Aeson
import qualified Data.Map.Strict              as Map
import           Data.Text                    (Text)
import           Text.Mustache.Internal.Types


-- | Convenience function for creating Object values.
--
-- This function is supposed to be used in conjuction with the '~>' and '~=' operators.
--
-- ==== __Examples__
--
-- @
--   data Address = Address { ... }
--
--   instance Address ToJSON where
--     ...
--
--   data Person = Person { name :: String, address :: Address }
--
--   instance ToMustache Person where
--     toMustache (Person { name, address }) = object
--       [ "name" ~> name
--       , "address" ~= address
--       ]
-- @
--
-- Here we can see that we can use the '~>' operator for values that have
-- themselves a 'ToMustache' instance, or alternatively if they lack such an
-- instance but provide an instance for the 'ToJSON' typeclass we can use the
-- '~=' operator.
object :: [Pair] -> Value
object = Object . Map.fromList


-- | Map keys to values that provide a 'ToMustache' instance
--
-- Recommended in conjunction with the `OverloadedStrings` extension.
(~>) :: ToMustache ω => Text -> ω -> Pair
(~>) t = (t, ) . toMustache
{-# INLINEABLE (~>) #-}
infixr 8 ~>

-- | Unicode version of '~>'
(↝) :: ToMustache ω => Text -> ω -> Pair
(↝) = (~>)
{-# INLINEABLE (↝) #-}
infixr 8 ↝


-- | Map keys to values that provide a 'ToJSON' instance
--
-- Recommended in conjunction with the `OverloadedStrings` extension.
(~=) :: Aeson.ToJSON ι => Text -> ι -> Pair
(~=) t = (t ~>) . Aeson.toJSON
{-# INLINEABLE (~=) #-}
infixr 8 ~=


-- | Unicode version of '~='
(⥱) :: Aeson.ToJSON ι => Text -> ι -> Pair
(⥱) = (~=)
{-# INLINEABLE (⥱) #-}
infixr 8 ⥱


-- | Converts a value that can be represented as JSON to a Value.
mFromJSON :: Aeson.ToJSON ι => ι -> Value
mFromJSON = toMustache . Aeson.toJSON


askContext :: SubM (Context Value)
askContext = asks fst


askPartials :: SubM TemplateCache
askPartials = asks snd
