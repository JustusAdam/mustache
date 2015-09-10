{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Text.Mustache.Types
  ( MustacheAST
  , MustacheNode(..)
  , MustacheValue(..)
  , ToMustache, toMustache
  , Array, Object
  , MustacheTemplate(..)
  , object
  , (.=), (.<>)
  , Context(..)
  ) where


import qualified Data.Aeson          as Aeson
import           Data.HashMap.Strict as HM
import           Data.Scientific
import           Data.Text
import qualified Data.Text.Lazy      as LT
import qualified Data.Vector         as V


-- Abstract syntax tree for a mustache template
type MustacheAST = [MustacheNode Text]


-- Basic values composing the AST
data MustacheNode a
  = MustacheText a
  | MustacheSection [Text] MustacheAST
  | MustacheInvertedSection [Text] MustacheAST
  | MustacheVariable Bool [Text]
  | MustachePartial FilePath
  deriving (Show, Eq)



type Array = V.Vector MustacheValue
type Object = HM.HashMap Text MustacheValue

data Context a = Context [a] a


data MustacheValue
  = Object Object
  | Array Array
  | Number Scientific
  | String Text
  | Lambda (Context MustacheValue → MustacheAST → Either String MustacheAST)
  | Bool Bool
  | Null


instance Show MustacheValue where
  show (Lambda _)  = "Lambda Text -> Text"
  show (Object o)  = show o
  show (Array a)   = show a
  show (String s)  = show s
  show (Number n)  = show n
  show (Bool b)    = show b
  show Null        = "null"


class ToMustache a where
  toMustache ∷ a → MustacheValue


instance ToMustache Aeson.Value where
  toMustache = fromJson

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

instance ToMustache MustacheValue where
  toMustache = id

instance ToMustache m ⇒ ToMustache [m] where
  toMustache = Array . V.fromList . fmap toMustache

instance ToMustache m ⇒ ToMustache (V.Vector m) where
  toMustache = Array . fmap toMustache

instance ToMustache m ⇒ ToMustache (HM.HashMap Text m) where
  toMustache = Object . fmap toMustache

instance ToMustache (Context MustacheValue → MustacheAST → Either String MustacheAST) where
  toMustache = Lambda

instance ToMustache (Context MustacheValue → MustacheAST → Either String Text) where
  toMustache f = Lambda wrapper
    where wrapper c lAST = return . MustacheText <$> f c lAST

instance ToMustache (Context MustacheValue → MustacheAST → MustacheAST) where
  toMustache f = Lambda wrapper
    where wrapper c = Right . f c

instance ToMustache (Context MustacheValue → MustacheAST → Text) where
  toMustache f = Lambda wrapper
    where wrapper c = Right . return . MustacheText . f c

instance ToMustache (Context MustacheValue → MustacheAST → String) where
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
  toMustache f = toMustache (Right . f :: MustacheAST -> Either String Text)

instance ToMustache (MustacheAST → String) where
  toMustache f = toMustache (pack . f)

fromJson ∷ Aeson.Value → MustacheValue
fromJson (Aeson.Object o) = Object $ fmap fromJson o
fromJson (Aeson.Array a)  = Array $ fmap fromJson a
fromJson (Aeson.Number n) = Number n
fromJson (Aeson.String s) = String s
fromJson (Aeson.Bool b)   = Bool b
fromJson (Aeson.Null)     = Null


object ∷ [(Text, MustacheValue)] → MustacheValue
object = Object . HM.fromList


(.=) ∷ ToMustache m ⇒ Text → m → (Text, MustacheValue)
(.=) t = (t,) . toMustache


(.<>) ∷ Aeson.ToJSON j ⇒ Text → j → (Text, MustacheValue)
(.<>) t = (t .=) . Aeson.toJSON


{-|
  A compiled Template with metadata.
-}
data MustacheTemplate = MustacheTemplate { name     ∷ String
                                         , ast      ∷ MustacheAST
                                         , partials ∷ [MustacheTemplate]
                                         } deriving (Show)
