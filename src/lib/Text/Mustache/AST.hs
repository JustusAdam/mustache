module Text.Mustache.AST
  ( MustacheAST
  , MustacheNode(..)
  ) where


import Data.Text


-- Abstract syntax tree for a mustache template
type MustacheAST = [MustacheNode Text]


-- Basic values composing the AST
data MustacheNode a
  = MustacheText a
  | MustacheSection Text MustacheAST
  | MustacheInvertedSection Text MustacheAST
  | MustacheVariable Bool Text
  | MustachePartial FilePath
  deriving (Show, Eq)
