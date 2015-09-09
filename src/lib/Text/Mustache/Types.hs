module Text.Mustache.Types
  ( MustacheAST
  , MustacheNode(..)
  , MustacheData(..)
  , ToMustache, toMustache
  , Array, Object
  , MustacheTemplate(..)
  ) where


import qualified Data.Aeson          as Aeson
import           Data.HashMap.Strict as HM
import           Data.Scientific
import           Data.Text
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



type Array = V.Vector MustacheData
type Object = HM.HashMap Text MustacheData


data MustacheData
  = Object Object
  | Array Array
  | Number Scientific
  | String Text
  | Lambda (Text -> Text)
  | Bool Bool
  | Null


instance Show MustacheData where
  show (Lambda _)  = "Lambda Text -> Text"
  show (Object o)  = show o
  show (Array a)   = show a
  show (String s)  = show s
  show (Number n)  = show n
  show (Bool b)    = show b
  show Null        = "null"



class ToMustache a where
  toMustache :: a -> MustacheData


instance ToMustache Aeson.Value where
  toMustache = fromJson


fromJson :: Aeson.Value -> MustacheData
fromJson (Aeson.Object o) = Object $ fmap fromJson o
fromJson (Aeson.Array a)  = Array $ fmap fromJson a
fromJson (Aeson.Number n) = Number n
fromJson (Aeson.String s) = String s
fromJson (Aeson.Bool b)   = Bool b
fromJson (Aeson.Null)     = Null


{-|
  A compiled Template with metadata.
-}
data MustacheTemplate = MustacheTemplate { name     :: String
                                         , ast      :: MustacheAST
                                         , partials :: [MustacheTemplate]
                                         } deriving (Show)
