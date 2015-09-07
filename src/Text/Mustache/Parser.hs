{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Mustache.Parser
  ( ParseError
  , mustacheParser
  ) where


import           Control.Monad.Error.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Data.Bool
import           Data.Function
import           Data.Monoid
import           Data.Text
import           Text.Mustache.AST
import           Text.Parsec as P hiding (try)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token
import           Text.Printf


data MustacheConf = MustacheConf { delimiters :: (String, String) }


sectionBegin = "#"
sectionEnd = "/"
partialBegin = ">"
invertedSectionBegin = "^"
unescape2 = ("{", "}")
unescape1 = "&"
delimiterChange = "="


mustacheAllowedCharacters =
  choice $
    alphaNum
    : fmap char "-_"


emptyConf = MustacheConf { delimiters = ("", "") }


type MustacheParser = GenParser Char MustacheConf


mustacheParser :: FilePath -> String -> Either ParseError MustacheAST
mustacheParser =
  P.runParser (mustacheParseNode Nothing) (emptyConf { delimiters = ("{{", "}}") })


parseTag :: String -> String -> MustacheParser String
parseTag start end = do
  string start
  skipMany space
  manyTill mustacheAllowedCharacters (try (skipMany space >> string end))



mustacheParseNode :: Maybe String -> MustacheParser MustacheAST
mustacheParseNode name = do
  (MustacheConf { delimiters =  (start, end) }) <- getState
  decide start end

  where
    decide start end =
      choice
        [ try isEnd
        , try isBegin >>= continue
        , try isInverted >>= continue
        , try isUnescaped >>= continue
      -- <|> (isDelimiterChange >>= continue)
        , try isPartial >>= continue
        , try isVariable >>= continue
        , try eof >> maybe (return []) (parserFail . ("Unclosed section " <>)) name
        , isText
        ]
      where
        isSameSection = maybe (const True) (==) name

        continue val = (val :) <$> decide start end

        isTag smod emod = parseTag (start <> smod) (emod <> end)

        isEnd = isTag sectionEnd mempty >>= \tag ->
          bool
            (parserFail $
              maybe
                (unexpectedSection tag)
                (`unexpectedClosingSequence` tag)
                name
            )
            (return [])
          (isSameSection tag)

        isVariable = MustacheVariable True <$> isTag mempty mempty
        isInverted = isTag invertedSectionBegin mempty >>= \sectionName ->
          MustacheInvertedSection sectionName <$> mustacheParseNode (return sectionName)
        isUnescaped = MustacheVariable False <$> (try (uncurry isTag unescape2) <|> isTag unescape1 mempty)
        -- isDelimiterChange = do
        --   contents <- isTag delimiterChange delimiterChange
        isPartial = MustachePartial <$> isTag partialBegin mempty

        isBegin =
          isTag sectionBegin mempty >>= \sectionName ->
            MustacheSection sectionName <$> mustacheParseNode (return sectionName)

        isText =
          try anyChar >>= \c ->
            (<$> mustacheParseNode name) $ \case
              (MustacheText tx : r) -> MustacheText (c : tx) : r
              r -> MustacheText [c] : r


-- ERRORS

unexpectedSection = printf "No such section '%s'"
unexpectedClosingSequence :: String -> String -> String
unexpectedClosingSequence = printf "Expected closing sequence for section '%s' got '%s'"
