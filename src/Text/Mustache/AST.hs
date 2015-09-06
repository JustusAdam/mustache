module Text.Mustache.AST where


import Data.Text


type MustacheAST = [MustacheNode]


data MustacheNode
  = MustacheText String
  | MustacheSection String MustacheAST
  | MustacheInvertedSection String
  | MustacheVariable Bool String
  | MustachePartial String
