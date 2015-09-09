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
import           Data.Monoid
import           Data.Text         as T
import           Text.Mustache.AST
import           Text.Parsec       as P hiding (parse)
import           Text.Printf


data MustacheConf = MustacheConf
  { delimiters      :: (String, String)
  , dotNavigation   :: Bool
  , bracketIndexing :: Bool
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
isAllowedDelimiterCharacter = not . or . sequence [ isSpace, isAlphaNum ]
allowedDelimiterCharacter :: MustacheParser Char
allowedDelimiterCharacter =
  satisfy isAllowedDelimiterCharacter


mustacheAllowedCharacters :: MustacheParser Char
mustacheAllowedCharacters =
  choice $
    alphaNum
    : fmap char "-_"


emptyConf :: MustacheConf
emptyConf = MustacheConf ("", "") False False


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
  name = do
    (MustacheConf { delimiters = ( start, _ )}) <- getState
    let endOfText = try (void $ string start) <|> try eof
    content <- pack <$> manyTill anyChar (lookAhead endOfText)
    others <- parseTag name
    return $ if T.null content
              then others
              else MustacheText content : others



parseTag :: Maybe [Text] -> MustacheParser MustacheAST
parseTag name = choice
  [ parseEnd name >> return []
  , parseSection >>= continue
  , parseInvertedSection >>= continue
  , parseUnescapedVar >>= continue
  , parseDelimiterChange >> parseText name
  , parsePartial >>= continue
  , parseVariable >>= continue
  , eof >> maybe (return []) (parserFail . ("Unclosed section " <>) . unpack . fold) name
  ]
  where
    continue val = (val :) <$> parseText name

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
    mustacheAllowedCharacters `manyTill` try (skipMany space >> string pEnd)


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
parseEnd name = do
  tag <- parseNavigation sectionEnd mempty
  unless (isSameSection tag) $
    parserFail $
      maybe
        (unexpectedSection tag)
        (`unexpectedClosingSequence` tag)
        name
  where
    isSameSection = maybe (const True) (==) name


parseNavigation :: String -> String -> MustacheParser [Text]
parseNavigation smod emod = do
  (MustacheConf { delimiters = ( start, end ), dotNavigation = dotNav }) <- getState
  let nStart = start <> smod
      nEnd = emod <> end
  void $ try $ string nStart
  parseOne dotNav nStart nEnd
  where
    parseOne dotNav nStart nEnd = do
      spaces
      one <- mustacheAllowedCharacters `manyTill` (try spaces >> lookAhead (try (void $ string nEnd) <|> dotNavigationEndModifier))
      others <- dotNavigationParseContinuation <|> (const mempty <$> string nEnd)
      return $ pack one : others
      where
        dotNavigationEndModifier =
          if dotNav
            then try (void $ char '.')
            else mzero
        dotNavigationParseContinuation =
          if dotNav
            then char '.' >> parseOne dotNav nStart nEnd
            else mzero


-- ERRORS

unexpectedSection :: [Text] -> String
unexpectedSection = printf "No such section '%s'" . unpack . fold
unexpectedClosingSequence :: [Text] -> [Text] -> String
unexpectedClosingSequence tag1 tag2 =
  "Expected closing sequence for section '"
  <> unpack (fold tag1)
  <> "' got '"
  <> unpack (fold tag2)
  <> "'"
