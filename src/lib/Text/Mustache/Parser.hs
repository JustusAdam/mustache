{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.Mustache.Parser
  ( ParseError
  , mustacheParser
  ) where


import           Data.Bool
import           Data.Char
import           Data.Functor
import           Data.Monoid
import           Data.Text
import           Text.Mustache.AST
import           Text.Parsec         as P
import           Text.Printf


data MustacheConf = MustacheConf { delimiters :: (String, String) }


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
allowedDelimiterCharacter :: MustacheParser Char
allowedDelimiterCharacter =
  satisfy $ not . or . sequence [ isSpace, isAlphaNum ]


mustacheAllowedCharacters :: MustacheParser Char
mustacheAllowedCharacters =
  choice $
    alphaNum
    : fmap char "-_"


emptyConf :: MustacheConf
emptyConf = MustacheConf { delimiters = ("", "") }


type MustacheParser = Parsec Text MustacheConf


{-|
  Runs the parser for a mustache template, returning the syntax tree.
-}
mustacheParser :: FilePath -> Text -> Either ParseError MustacheAST
mustacheParser =
  P.runParser (mustacheParseNode Nothing) (emptyConf { delimiters = ("{{", "}}") })


parseTag :: String -> String -> MustacheParser String
parseTag start end = do
  void $ string start
  spaces
  mustacheAllowedCharacters `manyTill` try (skipMany space >> string end)



mustacheParseNode :: Maybe Text -> MustacheParser MustacheAST
mustacheParseNode name = do
  (MustacheConf { delimiters =  (start, end) }) <- getState
  decide start end

  where
    decide :: String -> String -> MustacheParser MustacheAST
    decide start end =
      choice
        [ try isEnd
        , try isBegin >>= continue
        , try isInverted >>= continue
        , try isUnescaped >>= continue
        , try isDelimiterChange
        , try isPartial >>= continue
        , try isVariable >>= continue
        , try eof >> maybe (return []) (parserFail . ("Unclosed section " <>) . unpack) name
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
                (flip unexpectedClosingSequence tag . unpack)
                name
            )
            (return [])
          (isSameSection (pack tag))

        isVariable = MustacheVariable True . pack <$> isTag mempty mempty
        isInverted = pack <$> isTag invertedSectionBegin mempty >>= \sectionName ->
          MustacheInvertedSection sectionName <$> mustacheParseNode (return sectionName)
        isUnescaped = MustacheVariable False . pack <$> (try (uncurry isTag unescape2) <|> isTag unescape1 mempty)

        isDelimiterChange = do
          void $ string (start <> delimiterChange)
          delim1 <- allowedDelimiterCharacter `manyTill` space
          spaces
          delim2 <- allowedDelimiterCharacter `manyTill` try (string $ delimiterChange <> end)
          oldState <- getState
          putState $ oldState { delimiters = (delim1, delim2) }
          mustacheParseNode name

        isPartial = MustachePartial <$> isTag partialBegin mempty

        isBegin =
          pack <$> isTag sectionBegin mempty >>= \sectionName ->
            MustacheSection sectionName <$> mustacheParseNode (return sectionName)

        isText =
          try anyChar >>= \c' ->
            let
              c = pack [c']
            in
              (<$> mustacheParseNode name) $ \case
                (MustacheText tx : r) -> MustacheText (c <> tx) : r
                r -> MustacheText c : r


-- ERRORS

unexpectedSection :: String -> String
unexpectedSection = printf "No such section '%s'"
unexpectedClosingSequence :: String -> String -> String
unexpectedClosingSequence = printf "Expected closing sequence for section '%s' got '%s'"
