{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Text.Mustache.Parser where


import Text.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec
import Data.Text
import Data.Function
import Text.Mustache.AST
import Data.Monoid
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Data.Bool
import Control.Monad.Trans
import Text.Printf


data MustacheConf = MustacheConf { delimiters :: (String, String) }


sectionBegin = "#" :: String
sectionEnd = "/" :: String
partialBegin = ">" :: String
invertedSectionBegin = "^" :: String
unescape2 = ("{", "}") :: (String, String)
unescape1 = "&" :: String
delimiterChange = "=" :: String


emptyConf = MustacheConf { delimiters = ("", "") }


type MustacheParser = GenParser Char MustacheConf


mustacheParser :: MustacheParser MustacheAST
mustacheParser = do
  putState $ emptyConf { delimiters = ("{{", "}}") }
  mustacheParseNode Nothing


mustacheParseNode :: Maybe String -> MustacheParser MustacheAST
mustacheParseNode name = do
  (MustacheConf { delimiters =  (start, end) }) <- getState
  decide start end

  where
    decide start end =
      isEnd
      <|> (isBegin >>= continue)
      <|> (isInverted >>= continue)
      <|> (isUnescaped >>= continue)
      -- <|> (isDelimiterChange >>= continue)
      <|> (isPartial >>= continue)
      <|> (isVariable >>= continue)
      <|> isText
      where
        isSameSection = maybe (const True) (==) name

        continue val = (val :) <$> mustacheParseNode name

        isTag :: String -> String -> MustacheParser String
        isTag smod emod = between (string $ start <> smod) (string $ emod <> end) (many anyChar)

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

        isVariable = MustacheVariable False <$> isTag mempty mempty
        isInverted = MustacheInvertedSection <$> isTag invertedSectionBegin mempty
        isUnescaped = MustacheVariable True <$> (uncurry isTag unescape2 <|> isTag unescape1 mempty)
        -- isDelimiterChange = do
        --   contents <- isTag delimiterChange delimiterChange
        isPartial = MustachePartial <$> isTag partialBegin mempty

        isBegin =
          isTag sectionBegin mempty >>= \sectionName ->
            MustacheSection sectionName <$> mustacheParseNode (return sectionName)

        isText =
          anyChar >>= \c ->
            (<$> mustacheParseNode name) $ \case
              (MustacheText tx : r) -> MustacheText (c : tx) : r
              r -> MustacheText [c] : r


-- ERRORS

unexpectedSection = printf "No such section '%s'"
unexpectedClosingSequence :: String -> String -> String
unexpectedClosingSequence = printf "Expected closing sequence for section '%s' got '%s'"
