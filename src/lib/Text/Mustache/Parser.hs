{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Mustache.Parser
  (
  -- * Generic parser

    mustacheParser

  -- * Mustache Constants

  , sectionBegin, sectionEnd, invertedSectionBegin, unescape2, unescape1
  , delimiterChange

  -- * Parser configurations

  , MustacheConf, emptyConf, defaultConf

  -- * Parser util

  , MustacheParser, parseTag, mustacheParseNode
  ) where


import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.Text as T
import           Text.Mustache.AST
import           Text.Parsec       as P
import           Text.Printf
import Control.Monad


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
mustacheParser :: FilePath -> Text -> Either ParseError MustacheAST
mustacheParser =
  P.runParser (mustacheParseNode Nothing) defaultConf


parseTag :: String -> String -> MustacheParser String
parseTag start end = do
  void $ string start
  spaces
  mustacheAllowedCharacters `manyTill` try (skipMany space >> string end)


mustacheParseNode :: Maybe [Text] -> MustacheParser MustacheAST
mustacheParseNode
  name = do
    (MustacheConf { delimiters = ( start, _ )}) <- getState
    let endOfText = try (void $ string start) <|> try eof
    content <- pack <$> manyTill anyChar (lookAhead endOfText)
    others <- choice
      [ try (parseEnd name)
      , try parseSection >>= continue
      , try parseInvertedSection >>= continue
      , try parseUnescapedVar >>= continue
      , try (parseDelimiterChange name)
      , try parsePartial >>= continue
      , try parseVariable >>= continue
      , eof >> maybe (return []) (parserFail . ("Unclosed section " <>) . unpack . fold) name
      ]
    return $ if T.null content
              then others
              else MustacheText content : others
  where
    continue val = (val :) <$> mustacheParseNode name


parseSection :: MNodeParser
parseSection = do
  sectionName <- parseNavigation sectionBegin mempty
  MustacheSection sectionName <$> mustacheParseNode (return sectionName)


parsePartial :: MNodeParser
parsePartial = do
  (MustacheConf { delimiters = ( start, end )}) <- getState
  MustachePartial <$> parseTag (start <> partialBegin) end


parseDelimiterChange :: Maybe [Text] -> MustacheParser MustacheAST
parseDelimiterChange name = do
  (MustacheConf { delimiters = ( start, end )}) <- getState
  void $ string (start <> delimiterChange)
  delim1 <- allowedDelimiterCharacter `manyTill` space
  spaces
  delim2 <- allowedDelimiterCharacter `manyTill` try (string $ delimiterChange <> end)
  oldState <- getState
  putState $ oldState { delimiters = (delim1, delim2) }
  mustacheParseNode name


parseInvertedSection :: MNodeParser
parseInvertedSection = do
  sectionName <- parseNavigation invertedSectionBegin mempty
  MustacheInvertedSection sectionName <$> mustacheParseNode (return sectionName)


parseUnescapedVar :: MNodeParser
parseUnescapedVar = MustacheVariable False <$>
  (try (uncurry parseNavigation unescape2) <|> parseNavigation unescape1 mempty)


parseVariable :: MNodeParser
parseVariable = MustacheVariable True <$> parseNavigation mempty mempty


parseEnd :: Maybe [Text] -> MustacheParser MustacheAST
parseEnd name = do
  tag <- parseNavigation sectionEnd mempty
  if isSameSection tag
    then return []
    else parserFail $
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
  void $ string nStart
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
