{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UnicodeSyntax         #-}
module Text.Mustache.Parser
  (
  -- * Generic parsing functions

    parse, parseWithConf

  -- * Configurations

  , MustacheConf, defaultConf

  -- * Parser

  , MustacheParser

  -- ** Components

  , parseText

  -- * Mustache Constants

  , sectionBegin, sectionEnd, invertedSectionBegin, unescape2, unescape1
  , delimiterChange, nestingSeparator

  ) where


import           Control.Monad
import           Conversion          (Conversion, convert)
import           Conversion.Text     ()
import           Data.Char           (isAlphaNum, isSpace)
import           Data.Foldable       (fold)
import           Data.Functor        ((<$>))
import           Data.List           (nub)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mempty, (<>))
import           Data.Text           as T (Text, pack, null, intercalate, unpack)
import           Prelude             as Prel
import           Text.Mustache.Types
import           Text.Parsec         as P hiding (parse)


data MustacheConf = MustacheConf
  { delimiters ∷ (String, String)
  }


data MustacheState = MustacheState
  { sDelimiters        ∷ (String, String)
  , textStack          ∷ Text
  , isBeginngingOfLine ∷ Bool
  , currentSectionName ∷ Maybe [Text]
  }

data ParseTagRes
  = SectionBegin Bool [Text]
  | SectionEnd [Text]
  | Tag (MustacheNode Text)
  | HandledTag

-- | @#@
sectionBegin ∷ Char
sectionBegin = '#'
-- | @/@
sectionEnd ∷ Char
sectionEnd = '/'
-- | @>@
partialBegin ∷ Char
partialBegin = '>'
-- | @^@
invertedSectionBegin ∷ Char
invertedSectionBegin = '^'
-- | @{@ and @}@
unescape2 ∷ (Char, Char)
unescape2 = ('{', '}')
-- | @&@
unescape1 ∷ Char
unescape1 = '&'
-- | @=@
delimiterChange ∷ Char
delimiterChange = '='
-- | @.@
nestingSeparator ∷ Char
nestingSeparator = '.'
comment ∷ Char
comment = '!'
-- | Cannot be a letter, number or the nesting separation Character @.@
isAllowedDelimiterCharacter ∷ Char → Bool
isAllowedDelimiterCharacter =
  not . Prel.or . sequence
    [ isSpace, isAlphaNum, (== nestingSeparator) ]
allowedDelimiterCharacter ∷ MustacheParser Char
allowedDelimiterCharacter =
  satisfy isAllowedDelimiterCharacter


-- | Empty configuration
emptyState ∷ MustacheState
emptyState = MustacheState ("", "") mempty True Nothing


-- | Default configuration (delimiters = ("{{", "}}"))
defaultConf ∷ MustacheConf
defaultConf = MustacheConf ("{{", "}}")


initState ∷ MustacheConf -> MustacheState
initState (MustacheConf { delimiters }) = emptyState { sDelimiters = delimiters }


type MustacheParser = Parsec Text MustacheState


(<<) ∷ Monad m ⇒ m b → m a → m b
(<<) = flip (>>)


customEndOfLine ∷ MustacheParser String
customEndOfLine =
  try (return <$> char '\n')
  <|> string "\r\n"


{-|
  Runs the parser for a mustache template, returning the syntax tree.
-}
parse ∷ FilePath → Text → Either ParseError MustacheAST
parse = parseWithConf defaultConf


parseWithConf ∷ MustacheConf → FilePath → Text → Either ParseError MustacheAST
parseWithConf = P.runParser parseText . initState


parseText ∷ MustacheParser MustacheAST
parseText = do
  (MustacheState { isBeginngingOfLine }) ← getState
  if isBeginngingOfLine
    then parseLine
    else continueLine


appendTextStack ∷ Conversion t Text ⇒ t → MustacheParser ()
appendTextStack t = modifyState (\s → s { textStack = textStack s <> convert t})


continueLine ∷ MustacheParser MustacheAST
continueLine = do
  (MustacheState { sDelimiters = ( start, _ )}) ← getState
  let forbidden = head start : "\n\r"

  many (noneOf forbidden) >>= appendTextStack

  (try customEndOfLine >>= appendTextStack >> parseLine)
    <|> (try (string start) >> switchOnTag >>= continueFromTag)
    <|> (try eof >> finishFile)
    <|> (anyChar >>= appendTextStack >> continueLine)


flushText ∷ MustacheParser MustacheAST
flushText = do
  s@(MustacheState { textStack = text }) ← getState
  putState $ s { textStack = mempty }
  return $ if T.null text
              then []
              else return $ MustacheText text


finishFile ∷ MustacheParser MustacheAST
finishFile =
  getState >>= \case
    (MustacheState {currentSectionName = Nothing}) → flushText
    (MustacheState {currentSectionName = Just name}) →
      parserFail $ "Unclosed section " <> sectionToString name


parseLine ∷ MustacheParser MustacheAST
parseLine = do
  (MustacheState { sDelimiters = ( start, _ ) }) ← getState
  initialWhitespace ← many (oneOf " \t")
  let handleStandalone = do
        tag ← switchOnTag
        let continueNoStandalone = appendTextStack initialWhitespace >> continueFromTag tag
        case tag of
          Tag _ → continueNoStandalone
          _ →
            ( do
              try (skipMany (oneOf " \t"))
              try eof <|> void (try customEndOfLine)
              continueFromTag tag
            )
              <|> continueNoStandalone
  (try (string start) >> handleStandalone)
    <|> (try eof >> appendTextStack initialWhitespace >> finishFile)
    <|> (appendTextStack initialWhitespace >> continueLine)


continueFromTag ∷ ParseTagRes → MustacheParser MustacheAST
continueFromTag (SectionBegin inverted name) = do
  textNodes ← flushText
  state@(MustacheState
    { currentSectionName = previousSection }) ← getState
  putState $ state { currentSectionName = Just name }
  innerSectionContent ← parseText
  let sectionTag =
        if inverted
          then MustacheInvertedSection
          else MustacheSection
  modifyState $ \s → s { currentSectionName = previousSection }
  outerSectionContent ← parseText
  return (textNodes <> [sectionTag name innerSectionContent] <> outerSectionContent)
continueFromTag (SectionEnd name) = do
  (MustacheState
    { currentSectionName }) ← getState
  case currentSectionName of
    Just cName | name == cName → flushText
    Just _  → parserFail $ wrongClosingSequence (fromMaybe ["<root>"] currentSectionName) name
    Nothing → parserFail $ unexpectedClosingSequence name
continueFromTag (Tag tag) = do
  textNodes    ← flushText
  furtherNodes ← parseText
  return $ textNodes <> return tag <> furtherNodes
continueFromTag HandledTag = continueLine


switchOnTag ∷ MustacheParser ParseTagRes
switchOnTag = do
  (MustacheState { sDelimiters = ( start, end )}) ← getState

  choice
    [ SectionBegin False
        <$> (try (char sectionBegin) >> genParseTagEnd mempty)
    , SectionEnd
        <$> (try (char sectionEnd) >> genParseTagEnd mempty)
    , Tag . MustacheVariable False
        <$> (try (char unescape1) >> genParseTagEnd mempty)
    , Tag . MustacheVariable False
        <$> (try (char (fst unescape2)) >> genParseTagEnd (return $ snd unescape2))
    , Tag . MustachePartial
        <$> (try (char partialBegin) >> spaces >> (noneOf (nub $ start <> end) `manyTill` (spaces >> string end)))
    , return HandledTag
        << (try (char delimiterChange) >> parseDelimChange)
    , SectionBegin True
        <$> (try (char invertedSectionBegin) >> genParseTagEnd mempty)
    , return HandledTag << (try (char comment) >> manyTill anyChar (try $ string end))
    , Tag . MustacheVariable True
        <$> genParseTagEnd mempty
    ]
  where
    parseDelimChange = do
      (MustacheState { sDelimiters = ( _, end )}) ← getState
      spaces
      delim1 ← allowedDelimiterCharacter `manyTill` space
      spaces
      delim2 ← allowedDelimiterCharacter `manyTill` try (spaces >> string (delimiterChange : end))
      when (delim1 == mempty || delim2 == mempty)
        $ parserFail "Tags must contain more than 0 characters"
      oldState ← getState
      putState $ oldState { sDelimiters = (delim1, delim2) }


genParseTagEnd ∷ String → MustacheParser [Text]
genParseTagEnd emod = do
  (MustacheState { sDelimiters = ( start, end ) }) ← getState

  let nEnd = emod <> end
      disallowed = nub $ nestingSeparator : start <> end

      parseOne :: MustacheParser [Text]
      parseOne = do
        spaces

        one ← noneOf disallowed
          `manyTill` lookAhead
            (try (spaces >> void (string nEnd))
            <|> try (void $ char nestingSeparator))

        others ← (char nestingSeparator >> parseOne)
                  <|> (const mempty <$> (spaces >> string nEnd))
        return $ pack one : others
  parseOne


-- ERRORS

sectionToString ∷ [Text] → String
sectionToString = unpack . intercalate "."

unexpectedClosingSequence ∷ [Text] → String
unexpectedClosingSequence s = "No such section '" <> sectionToString s <> "'"
wrongClosingSequence ∷ [Text] → [Text] → String
wrongClosingSequence tag1 tag2 =
  "Expected closing sequence for section '"
  <> sectionToString tag1
  <> "' got '"
  <> sectionToString tag2
  <> "'"
