{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
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

  , MustacheConf(..), defaultConf

  -- * Parser

  , Parser, MustacheState

  -- * Mustache Constants

  , sectionBegin, sectionEnd, invertedSectionBegin, unescape2, unescape1
  , delimiterChange, nestingSeparator

  ) where


import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char              (isAlphaNum, isSpace)
import           Data.Functor           ((<$>))
import           Data.List              (nub)
import           Data.Monoid.Unicode    ((∅), (⊕))
import           Data.Text              as T (Text, null, pack)
import           Prelude                as Prel
import           Prelude.Unicode
import           Text.Mustache.Types
import           Text.Parsec            as P hiding (endOfLine, parse)


-- | Initial configuration for the parser
data MustacheConf = MustacheConf
  { delimiters ∷ (String, String)
  }


-- | User state for the parser
data MustacheState = MustacheState
  { sDelimiters        ∷ (String, String)
  , textStack          ∷ Text
  , isBeginngingOfLine ∷ Bool
  , currentSectionName ∷ Maybe DataIdentifier
  }


data ParseTagRes
  = SectionBegin Bool DataIdentifier
  | SectionEnd DataIdentifier
  | Tag (Node Text)
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
-- | @!@
comment ∷ Char
comment = '!'
-- | @.@
implicitIterator ∷ Char
implicitIterator = '.'
-- | Cannot be a letter, number or the nesting separation Character @.@
isAllowedDelimiterCharacter ∷ Char → Bool
isAllowedDelimiterCharacter =
  not ∘ Prel.or ∘ sequence
    [ isSpace, isAlphaNum, (≡ nestingSeparator) ]
allowedDelimiterCharacter ∷ Parser Char
allowedDelimiterCharacter =
  satisfy isAllowedDelimiterCharacter


-- | Empty configuration
emptyState ∷ MustacheState
emptyState = MustacheState ("", "") (∅) True Nothing


-- | Default configuration (delimiters = ("{{", "}}"))
defaultConf ∷ MustacheConf
defaultConf = MustacheConf ("{{", "}}")


initState ∷ MustacheConf → MustacheState
initState (MustacheConf { delimiters }) = emptyState { sDelimiters = delimiters }


setIsBeginning ∷ Bool → Parser ()
setIsBeginning b = modifyState (\s -> s { isBeginngingOfLine = b })


-- | The parser monad in use
type Parser = Parsec Text MustacheState


(<<) ∷ Monad m ⇒ m b → m a → m b
(<<) = flip (≫)


endOfLine ∷ Parser String
endOfLine = do
  r ← optionMaybe $ char '\r'
  n ← char '\n'
  return $ maybe id (:) r [n]


{-|
  Runs the parser for a mustache template, returning the syntax tree.
-}
parse ∷ FilePath → Text → Either ParseError STree
parse = parseWithConf defaultConf


-- | Parse using a custom initial configuration
parseWithConf ∷ MustacheConf → FilePath → Text → Either ParseError STree
parseWithConf = P.runParser parseText ∘ initState


parseText ∷ Parser STree
parseText = do
  (MustacheState { isBeginngingOfLine }) ← getState
  if isBeginngingOfLine
    then parseLine
    else continueLine


appendStringStack ∷ String → Parser ()
appendStringStack t = modifyState (\s → s { textStack = textStack s ⊕ pack t})


continueLine ∷ Parser STree
continueLine = do
  (MustacheState { sDelimiters = ( start@(x:_), _ )}) ← getState
  let forbidden = x : "\n\r"

  many (noneOf forbidden) ≫= appendStringStack

  (try endOfLine ≫= appendStringStack ≫ setIsBeginning True ≫ parseLine)
    <|> (try (string start) ≫ switchOnTag ≫= continueFromTag)
    <|> (try eof ≫ finishFile)
    <|> (anyChar ≫= appendStringStack . (:[]) ≫ continueLine)


flushText ∷ Parser STree
flushText = do
  s@(MustacheState { textStack = text }) ← getState
  putState $ s { textStack = (∅) }
  return $ if T.null text
              then []
              else [TextBlock text]


finishFile ∷ Parser STree
finishFile =
  getState ≫= \case
    (MustacheState {currentSectionName = Nothing}) → flushText
    (MustacheState {currentSectionName = Just name}) →
      parserFail $ "Unclosed section " ⊕ show name


parseLine ∷ Parser STree
parseLine = do
  (MustacheState { sDelimiters = ( start, _ ) }) ← getState
  initialWhitespace ← many (oneOf " \t")
  let handleStandalone = do
        tag ← switchOnTag
        let continueNoStandalone = do
              appendStringStack initialWhitespace
              setIsBeginning False
              continueFromTag tag
            standaloneEnding = do
              try (skipMany (oneOf " \t") ≫ (eof <|> void endOfLine))
              setIsBeginning True
        case tag of
          Tag (Partial _ name) →
            ( standaloneEnding ≫
              continueFromTag (Tag (Partial (Just (pack initialWhitespace)) name))
            ) <|> continueNoStandalone
          Tag _ → continueNoStandalone
          _     →
            ( standaloneEnding ≫
              continueFromTag tag
            ) <|> continueNoStandalone
  (try (string start) ≫ handleStandalone)
    <|> (try eof ≫ appendStringStack initialWhitespace ≫ finishFile)
    <|> (appendStringStack initialWhitespace ≫ setIsBeginning False ≫ continueLine)


continueFromTag ∷ ParseTagRes → Parser STree
continueFromTag (SectionBegin inverted name) = do
  textNodes ← flushText
  state@(MustacheState
    { currentSectionName = previousSection }) ← getState
  putState $ state { currentSectionName = return name }
  innerSectionContent ← parseText
  let sectionTag =
        if inverted
          then InvertedSection
          else Section
  modifyState $ \s → s { currentSectionName = previousSection }
  outerSectionContent ← parseText
  return (textNodes ⊕ [sectionTag name innerSectionContent] ⊕ outerSectionContent)
continueFromTag (SectionEnd name) = do
  (MustacheState
    { currentSectionName }) ← getState
  case currentSectionName of
    Just name' | name' ≡ name → flushText
    Just name' → parserFail $ "Expected closing sequence for \"" ⊕ show name ⊕ "\" got \"" ⊕ show name' ⊕ "\"."
    Nothing → parserFail $ "Encountered closing sequence for \"" ⊕ show name ⊕ "\" which has never been opened."
continueFromTag (Tag tag) = do
  textNodes    ← flushText
  furtherNodes ← parseText
  return $ textNodes ⊕ return tag ⊕ furtherNodes
continueFromTag HandledTag = parseText


switchOnTag ∷ Parser ParseTagRes
switchOnTag = do
  (MustacheState { sDelimiters = ( _, end )}) ← getState

  choice
    [ SectionBegin False <$> (try (char sectionBegin) ≫ genParseTagEnd (∅))
    , SectionEnd
        <$> (try (char sectionEnd) ≫ genParseTagEnd (∅))
    , Tag ∘ Variable False
        <$> (try (char unescape1) ≫ genParseTagEnd (∅))
    , Tag ∘ Variable False
        <$> (try (char (fst unescape2)) ≫ genParseTagEnd (return $ snd unescape2))
    , Tag ∘ Partial Nothing
        <$> (try (char partialBegin) ≫ spaces ≫ (noneOf (nub end) `manyTill` try (spaces ≫ string end)))
    , return HandledTag
        << (try (char delimiterChange) ≫ parseDelimChange)
    , SectionBegin True
        <$> (try (char invertedSectionBegin) ≫ genParseTagEnd (∅) ≫= \case
              n@(NamedData _) → return n
              _ → parserFail "Inverted Sections can not be implicit."
            )
    , return HandledTag << (try (char comment) ≫ manyTill anyChar (try $ string end))
    , Tag . Variable True
        <$> genParseTagEnd (∅)
    ]
  where
    parseDelimChange = do
      (MustacheState { sDelimiters = ( _, end )}) ← getState
      spaces
      delim1 ← allowedDelimiterCharacter `manyTill` space
      spaces
      delim2 ← allowedDelimiterCharacter `manyTill` try (spaces ≫ string (delimiterChange : end))
      when (delim1 ≡ (∅) ∨ delim2 ≡ (∅))
        $ parserFail "Tags must contain more than 0 characters"
      oldState ← getState
      putState $ oldState { sDelimiters = (delim1, delim2) }


genParseTagEnd ∷ String → Parser DataIdentifier
genParseTagEnd emod = do
  (MustacheState { sDelimiters = ( start, end ) }) ← getState

  let nEnd = emod ⊕ end
      disallowed = nub $ nestingSeparator : start ⊕ end

      parseOne :: Parser [Text]
      parseOne = do

        one ← noneOf disallowed
          `manyTill` lookAhead
            (try (spaces ≫ void (string nEnd))
            <|> try (void $ char nestingSeparator))

        others ← (char nestingSeparator ≫ parseOne)
                  <|> (const (∅) <$> (spaces ≫ string nEnd))
        return $ pack one : others
  spaces
  (try (char implicitIterator) ≫ spaces ≫ string nEnd ≫ return Implicit)
    <|> (NamedData <$> parseOne)
