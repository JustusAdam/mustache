{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Mustache.Parser
  (
  -- * Generic parsing functions

    parse, parseWithConf

  -- * Configurations

  , MustacheConf, emptyConf, defaultConf

  -- * Parser

  , MustacheParser

  -- ** Components



  -- * Mustache Constants

  , sectionBegin, sectionEnd, invertedSectionBegin, unescape2, unescape1
  , delimiterChange

  ) where


import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Text           as T
import           Prelude             as Prel
import           Text.Mustache.Types
import           Text.Parsec         as P hiding (parse)


data MustacheConf = MustacheConf
  { delimiters :: (String, String)
  }


sectionBegin :: String
sectionBegin = "#"
sectionEnd :: String
sectionEnd = "/"
partialBegin :: String
partialBegin = ">"
invertedSectionBegin :: String
invertedSectionBegin = "^"
unescape2 :: (String, String)
unescape2 = ("{", "}")
unescape1 :: String
unescape1 = "&"
delimiterChange :: String
delimiterChange = "="
isAllowedDelimiterCharacter :: Char -> Bool
isAllowedDelimiterCharacter = not . Prel.or . sequence [ isSpace, isAlphaNum ]
allowedDelimiterCharacter :: MustacheParser Char
allowedDelimiterCharacter =
  satisfy isAllowedDelimiterCharacter


emptyConf :: MustacheConf
emptyConf = MustacheConf ("", "")


defaultConf :: MustacheConf
defaultConf = emptyConf { delimiters = ("{{", "}}") }


type MustacheParser = Parsec Text MustacheConf
type MNodeParser = MustacheParser (MustacheNode Text)


{-|
  Runs the parser for a mustache template, returning the syntax tree.
-}
parse :: FilePath -> Text -> Either ParseError MustacheAST
parse = parseWithConf defaultConf


parseWithConf :: MustacheConf -> FilePath -> Text -> Either ParseError MustacheAST
parseWithConf = P.runParser (parseText Nothing)


parseText :: Maybe [Text] -> MustacheParser MustacheAST
parseText
  tagName = do
    (MustacheConf { delimiters = ( start, _ )}) <- getState
    let endOfText = try (void $ string start) <|> try eof
    content <- pack <$> manyTill anyChar (lookAhead endOfText)
    others <- parseTag tagName
    return $ if T.null content
              then others
              else MustacheText content : others



parseTag :: Maybe [Text] -> MustacheParser MustacheAST
parseTag tagName = choice
  [ parseEnd tagName >> return []
  , parseSection >>= continue
  , parseInvertedSection >>= continue
  , parseUnescapedVar >>= continue
  , parseDelimiterChange >> parseText tagName
  , parsePartial >>= continue
  , parseVariable >>= continue
  , eof >> maybe (return []) (parserFail . ("Unclosed section " <>) . unpack . fold) tagName
  ]
  where
    continue val = (val :) <$> parseText tagName

parseSection :: MNodeParser
parseSection = do
  sectionName <- parseNavigation sectionBegin mempty
  MustacheSection sectionName <$> parseText (return sectionName)


parsePartial :: MNodeParser
parsePartial = do
  (MustacheConf { delimiters = ( start, end )}) <- getState
  let pStart = start <> partialBegin
      pEnd = end
  void $ try $ string pStart
  spaces
  MustachePartial <$>
    anyChar `manyTill` try (skipMany space >> string pEnd)


parseDelimiterChange :: MustacheParser ()
parseDelimiterChange = do
  (MustacheConf { delimiters = ( start, end )}) <- getState
  void $ try $ string (start <> delimiterChange)
  delim1 <- allowedDelimiterCharacter `manyTill` space
  spaces
  delim2 <- allowedDelimiterCharacter `manyTill` try (string $ delimiterChange <> end)
  oldState <- getState
  putState $ oldState { delimiters = (delim1, delim2) }


parseInvertedSection :: MNodeParser
parseInvertedSection = do
  sectionName <- parseNavigation invertedSectionBegin mempty
  MustacheInvertedSection sectionName <$> parseText (return sectionName)


parseUnescapedVar :: MNodeParser
parseUnescapedVar = MustacheVariable False <$>
  (try (uncurry parseNavigation unescape2) <|> parseNavigation unescape1 mempty)


parseVariable :: MNodeParser
parseVariable = MustacheVariable True <$> parseNavigation mempty mempty


parseEnd :: Maybe [Text] -> MustacheParser ()
parseEnd tagName = do
  tag <- parseNavigation sectionEnd mempty
  unless (isSameSection tag) $
    parserFail $
      maybe
        (unexpectedSection tag)
        (`unexpectedClosingSequence` tag)
        tagName
  where
    isSameSection = maybe (const True) (==) tagName


parseNavigation :: String -> String -> MustacheParser [Text]
parseNavigation smod emod = do
  (MustacheConf { delimiters = ( start, end ) }) <- getState
  let nStart = start <> smod
      nEnd = emod <> end
  void $ try $ string nStart
  parseOne nStart nEnd
  where
    parseOne :: String -> String -> MustacheParser [Text]
    parseOne nStart nEnd = do
      spaces
      one <- anyChar `manyTill` lookAhead (try (spaces >> void (string nEnd)) <|> try (void $ char '.'))
      others <- (char '.' >> parseOne nStart nEnd) <|> (const mempty <$> (spaces >> string nEnd))
      return $ pack one : others


-- ERRORS

sectionToString :: [Text] -> String
sectionToString = unpack . intercalate "."

unexpectedSection :: [Text] -> String
unexpectedSection s = "No such section '" <> sectionToString s <> "'"
unexpectedClosingSequence :: [Text] -> [Text] -> String
unexpectedClosingSequence tag1 tag2 =
  "Expected closing sequence for section '"
  <> sectionToString tag1
  <> "' got '"
  <> sectionToString tag2
  <> "'"
