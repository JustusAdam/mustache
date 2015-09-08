module Text.Mustache.AST
  ( MustacheAST
  , MustacheNode(..)
  ) where


-- Abstract syntax tree for a mustache template
type MustacheAST = [MustacheNode String]


-- Basic values composing the AST
data MustacheNode a
  = MustacheText a
  | MustacheSection String MustacheAST
  | MustacheInvertedSection String MustacheAST
  | MustacheVariable Bool String
  | MustachePartial String
  deriving (Show, Eq)
