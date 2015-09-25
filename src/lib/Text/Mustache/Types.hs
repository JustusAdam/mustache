{-|
Module      : $Header$
Description : Types and conversions
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE FlexibleContexts  #-}
module Text.Mustache.Types
  (
  -- * Types for the Parser / Template
    MustacheAST
  , MustacheTemplate(..)
  , MustacheNode(..)
  -- * Types for the Substitution / Data
  , Value(..)
  -- ** Converting
  , object
  , (~>), (~=), (~~>), (~~=)
  , ToMustache, toMustache, toMustacheText, mFromJSON
  -- ** Representation
  , Array, Object
  , Context(..)
  ) where


import qualified Data.Aeson          as Aeson
import           Data.Functor        ((<$>))
import           Data.HashMap.Strict as HM
import           Data.Scientific
import           Data.Text
import qualified Data.Text.Lazy      as LT
import qualified Data.Vector         as V
import           Conversion
import           Conversion.Text ()


-- | Abstract syntax tree for a mustache template
type MustacheAST = [MustacheNode Text]


-- | Basic values composing the AST
data MustacheNode a
  = MustacheText a
  | MustacheSection [Text] MustacheAST
  | MustacheInvertedSection [Text] MustacheAST
  | MustacheVariable Bool [Text]
  | MustachePartial FilePath
  | MustacheImplicitIterator
  deriving (Show, Eq)


type Array = V.Vector Value
type Object = HM.HashMap Text Value
type KeyValuePair = (Text, Value)


-- | Representation of stateful context for the substitution process
data Context a = Context [a] a


-- | Internal value AST
data Value
  = Object Object
  | Array Array
  | Number Scientific
  | String Text
  | Lambda (Context Value → MustacheAST → Either String MustacheAST)
  | Bool Bool
  | Null


instance Show Value where
  show (Lambda _)  = "Lambda Context Value → MustacheAST → Either String MustacheAST"
  show (Object o)  = show o
  show (Array a)   = show a
  show (String s)  = show s
  show (Number n)  = show n
  show (Bool b)    = show b
  show Null        = "null"


-- | Conversion class
class ToMustache a where
  toMustache ∷ a → Value


instance ToMustache [Char] where
  toMustache = String . pack

instance ToMustache Bool where
  toMustache = Bool

instance ToMustache Char where
  toMustache = String . pack . return

instance ToMustache () where
  toMustache = const Null

instance ToMustache Text where
  toMustache = String

instance ToMustache LT.Text where
  toMustache = String . LT.toStrict

instance ToMustache Scientific where
  toMustache = Number

instance ToMustache Value where
  toMustache = id

instance ToMustache m ⇒ ToMustache [m] where
  toMustache = Array . V.fromList . fmap toMustache

instance ToMustache m ⇒ ToMustache (V.Vector m) where
  toMustache = Array . fmap toMustache

instance ToMustache m ⇒ ToMustache (HM.HashMap Text m) where
  toMustache = Object . fmap toMustache

instance ToMustache (Context Value → MustacheAST → Either String MustacheAST) where
  toMustache = Lambda

instance ToMustache (Context Value → MustacheAST → Either String Text) where
  toMustache f = Lambda wrapper
    where wrapper c lAST = return . MustacheText <$> f c lAST

instance ToMustache (Context Value → MustacheAST → MustacheAST) where
  toMustache f = Lambda wrapper
    where wrapper c = Right . f c

instance ToMustache (Context Value → MustacheAST → Text) where
  toMustache f = Lambda wrapper
    where wrapper c = Right . return . MustacheText . f c

instance ToMustache (Context Value → MustacheAST → String) where
  toMustache f = toMustache wrapper
    where wrapper c = pack . f c

instance ToMustache (MustacheAST → Either String MustacheAST) where
  toMustache f = Lambda $ const f

instance ToMustache (MustacheAST → Either String Text) where
  toMustache f = Lambda wrapper
    where wrapper _ = fmap (return . MustacheText) . f

instance ToMustache (MustacheAST → Either String String) where
  toMustache f = toMustache (fmap pack . f)

instance ToMustache (MustacheAST → Text) where
  toMustache f = toMustache (Right . f ∷ MustacheAST -> Either String Text)

instance ToMustache (MustacheAST → String) where
  toMustache f = toMustache (pack . f)

instance ToMustache Aeson.Value where
  toMustache (Aeson.Object o) = Object $ fmap toMustache o
  toMustache (Aeson.Array  a) = Array $ fmap toMustache a
  toMustache (Aeson.Number n) = Number n
  toMustache (Aeson.String s) = String s
  toMustache (Aeson.Bool   b) = Bool b
  toMustache (Aeson.Null)     = Null


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
-- Here we can see that we can use the '~>' operator for values that have themselves
-- a 'ToMustache' instance, or alternatively if they lack such an instance but provide
-- an instance for the 'ToJSON' typeclass we can use the '~=' operator.
object ∷ [(Text, Value)] → Value
object = Object . HM.fromList


-- | Map keys to values that provide a 'ToMustache' instance
--
-- Recommended in conjunction with the `OverloadedStrings` extension.
(~>) ∷ ToMustache m ⇒ Text → m → KeyValuePair
(~>) t = (t, ) . toMustache


-- | Map keys to values that provide a 'ToJSON' instance
--
-- Recommended in conjunction with the `OverloadedStrings` extension.
(~=) ∷ Aeson.ToJSON j ⇒ Text → j → KeyValuePair
(~=) t = (t ~>) . Aeson.toJSON


-- | Conceptually similar to '~>' but uses arbitrary String-likes as keys.
(~~>) ∷ (Conversion t Text, ToMustache m) ⇒ t → m → KeyValuePair
(~~>) = (~>) . convert


-- | Conceptually similar to '~=' but uses arbitrary String-likes as keys.
(~~=) ∷ (Conversion t Text, Aeson.ToJSON j) ⇒ t → j → KeyValuePair
(~~=) = (~=) . convert


-- | Converts arbitrary String-likes to Values
toMustacheText ∷ Conversion t Text ⇒ t → Value
toMustacheText = String . convert


-- | Converts a value that can be represented as JSON to a Value.
mFromJSON ∷ Aeson.ToJSON j ⇒ j → Value
mFromJSON = toMustache . Aeson.toJSON


{-|
  A compiled Template with metadata.
-}
data MustacheTemplate = MustacheTemplate { name     ∷ String
                                         , ast      ∷ MustacheAST
                                         , partials ∷ [MustacheTemplate]
                                         } deriving (Show)
