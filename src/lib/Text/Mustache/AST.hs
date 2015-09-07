module Text.Mustache.AST where


type MustacheAST = [MustacheNode String]


data MustacheNode a
  = MustacheText a
  | MustacheSection String MustacheAST
  | MustacheInvertedSection String MustacheAST
  | MustacheVariable Bool String
  | MustachePartial String
  deriving (Show, Eq)
