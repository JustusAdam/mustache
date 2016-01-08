{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
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

  , MustacheConf, defaultConf

  -- * MParser

  , MParser, MustacheState

  -- * Mustache Constants

  , sectionBegin, sectionEnd, invertedSectionBegin, unescape2, unescape1
  , delimiterChange, nestingSeparator

  ) where


import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char              (isAlphaNum, isSpace)
import           Data.Functor           ((<$>))
import           Data.List              (nub)
import qualified Data.Text              as T
import           Prelude                as Prel
import           Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import           Text.Mustache.Types
import qualified Data.Attoparsec.Text            as P hiding (endOfLine)
import  Control.Monad.Trans.State
import Control.Monad.Trans (lift)
import Control.Applicative


-- | Initial configuration for the parser
data MustacheConf = MustacheConf
  { delimiters :: (T.Text, T.Text)
  }


-- | User state for the parser
data MustacheState = MustacheState
  { sDelimiters        :: (T.Text, T.Text)
  , textStack          :: T.Text
  , isBeginngingOfLine :: Bool
  , currentSectionName :: Maybe DataIdentifier
  }


data ParseTagRes
  = SectionBegin Bool DataIdentifier
  | SectionEnd DataIdentifier
  | Tag (Node T.Text)
  | HandledTag


-- | @#@
sectionBegin :: Char
sectionBegin = '#'
-- | @/@
sectionEnd :: Char
sectionEnd = '/'
-- | @>@
partialBegin :: Char
partialBegin = '>'
-- | @^@
invertedSectionBegin :: Char
invertedSectionBegin = '^'
-- | @{@ and @}@
unescape2 :: (Char, Char)
unescape2 = ('{', '}')
-- | @&@
unescape1 :: Char
unescape1 = '&'
-- | @=@
delimiterChange :: Char
delimiterChange = '='
-- | @.@
nestingSeparator :: Char
nestingSeparator = '.'
-- | @!@
comment :: Char
comment = '!'
-- | @.@
implicitIterator :: Char
implicitIterator = '.'
-- | Cannot be a letter, number or the nesting separation Character @.@
isAllowedDelimiterCharacter :: Char -> Bool
isAllowedDelimiterCharacter =
  not  .  Prel.or  .  sequence
    [ isSpace, isAlphaNum, (== nestingSeparator) ]
allowedDelimiterCharacter :: P.Parser Char
allowedDelimiterCharacter =
  P.satisfy isAllowedDelimiterCharacter


-- | Empty configuration
emptyState :: MustacheState
emptyState = MustacheState ("", "") (∅) True Nothing


-- | Default configuration (delimiters = ("{{", "}}"))
defaultConf :: MustacheConf
defaultConf = MustacheConf ("{{", "}}")


initState :: MustacheConf -> MustacheState
initState (MustacheConf { delimiters }) = emptyState { sDelimiters = delimiters }


setIsBeginning :: Bool -> MParser ()
setIsBeginning b = modify (\s -> s { isBeginngingOfLine = b })


-- | The parser monad in use
type MParser = StateT MustacheState P.Parser


(<<) :: Monad m => m b -> m a -> m b
(<<) = (<*)


endOfLine :: P.Parser T.Text
endOfLine = P.string "\r\n" <|> P.string "\n"


{-|
  Runs the parser for a mustache template, returning the syntax tree.
-}
parse :: T.Text -> Either String STree
parse = parseWithConf defaultConf


-- | Parse using a custom initial configuration
parseWithConf :: MustacheConf -> T.Text -> Either String STree
parseWithConf = (handleErrors  . )  .  P.parse  .  evalStateT parseText  .  initState


handleErrors :: P.Result a -> Either String a
handleErrors (P.Done _ r) = return r
handleErrors (P.Partial c) = handleErrors $ c ""
handleErrors (P.Fail _ _ a) = Left a


parseText :: MParser STree
parseText = do
  (MustacheState { isBeginngingOfLine }) <- get
  if isBeginngingOfLine
    then parseLine
    else continueLine


appendStringStack :: T.Text -> MParser ()
appendStringStack t = modify (\s -> s { textStack = textStack s ⊕ t})


continueLine :: MParser STree
continueLine = do
  (MustacheState { sDelimiters = ( start, _ )}) <- get
  let forbidden = T.head start : "\n\r"

  lift (many (P.satisfy (P.notInClass forbidden))) >>= appendStringStack  .  T.pack

  (lift (P.try endOfLine) >>= appendStringStack >> setIsBeginning True >> parseLine)
    <|> (lift (P.string start) >> switchOnTag >>= continueFromTag)
    <|> (lift P.endOfInput >> finishFile)
    <|> (lift P.anyChar >>= appendStringStack . T.singleton >> continueLine)


flushText :: MParser STree
flushText = do
  s@(MustacheState { textStack = text }) <- get
  put $ s { textStack = (∅) }
  return $ if T.null text
              then []
              else [TextBlock text]


finishFile :: MParser STree
finishFile =
  get >>= \case
    (MustacheState {currentSectionName = Nothing}) -> flushText
    (MustacheState {currentSectionName = Just name}) ->
      fail $ "Unclosed section " ⊕ show name


parseLine :: MParser STree
parseLine = do
  (MustacheState { sDelimiters = ( start, _ ) }) <- get
  initialWhitespace <- lift $ T.pack <$> P.many' (P.satisfy $ P.inClass " \t")
  let handleStandalone = do
        tag <- switchOnTag
        let continueNoStandalone = do
              appendStringStack initialWhitespace
              setIsBeginning False
              continueFromTag tag
            standaloneEnding = do
              lift $ do
                P.try $ P.skipMany $ P.satisfy $ P.inClass " \t"
                P.endOfInput <|> void endOfLine
              setIsBeginning True
        case tag of
          Tag (Partial _ name) ->
            ( standaloneEnding >>
              continueFromTag (Tag (Partial (Just initialWhitespace) name))
            ) <|> continueNoStandalone
          Tag _ -> continueNoStandalone
          _     ->
            ( standaloneEnding >>
              continueFromTag tag
            ) <|> continueNoStandalone
  (lift (P.string start) >> handleStandalone)
    <|> do
      lift P.endOfInput
      appendStringStack initialWhitespace
      finishFile
    <|> do
      appendStringStack initialWhitespace
      setIsBeginning False
      continueLine


continueFromTag :: ParseTagRes -> MParser STree
continueFromTag (SectionBegin inverted name) = do
  textNodes <- flushText
  state@(MustacheState
    { currentSectionName = previousSection }) <- get
  put $ state { currentSectionName = return name }
  innerSectionContent <- parseText
  let sectionTag =
        if inverted
          then InvertedSection
          else Section
  modify $ \s -> s { currentSectionName = previousSection }
  outerSectionContent <- parseText
  return (textNodes ⊕ [sectionTag name innerSectionContent] ⊕ outerSectionContent)
continueFromTag (SectionEnd name) = do
  (MustacheState
    { currentSectionName }) <- get
  case currentSectionName of
    Just name' | name' == name -> flushText
    Just name' -> fail $ "Expected closing sequence for \"" ⊕ show name ⊕ "\" got \"" ⊕ show name' ⊕ "\"."
    Nothing -> fail $ "Encountered closing sequence for \"" ⊕ show name ⊕ "\" which has never been opened."
continueFromTag (Tag tag) = do
  textNodes    <- flushText
  furtherNodes <- parseText
  return $ textNodes ⊕ return tag ⊕ furtherNodes
continueFromTag HandledTag = parseText


switchOnTag :: MParser ParseTagRes
switchOnTag = do
  (MustacheState { sDelimiters = ( _, end )}) <- get

  P.choice
    [ SectionBegin False <$> (lift (P.char sectionBegin) >> genParseTagEnd (∅))
    , SectionEnd
        <$> (lift (P.char sectionEnd) >> genParseTagEnd (∅))
    , Tag  .  Variable False
        <$> (lift (P.char unescape1) >> genParseTagEnd (∅))
    , Tag  .  Variable False
        <$> do
          lift $ void $ P.char $ fst unescape2
          genParseTagEnd $ T.singleton $ snd unescape2
    , Tag  .  Partial Nothing
        <$> lift (do
          void $ P.char partialBegin
          P.skipSpace
          P.satisfy (P.notInClass (T.unpack end)) `P.manyTill` P.try (P.skipSpace >> P.string end)
        )
    , return HandledTag
        << (lift (P.char delimiterChange) >> parseDelimChange)
    , SectionBegin True
        <$> do
          lift $ void $ P.char invertedSectionBegin
          tagEnd <- genParseTagEnd (∅)
          case tagEnd of
              n@(NamedData _) -> return n
              _ -> fail "Inverted Sections can not be implicit."

    , return HandledTag << lift (P.char comment >> P.manyTill P.anyChar (P.string end))
    , Tag . Variable True
        <$> genParseTagEnd (∅)
    ]
  where
    parseDelimChange = do
      (MustacheState { sDelimiters = ( _, end )}) <- get
      (delim1, delim2) <- lift $ do
        P.skipSpace
        delim1 <- allowedDelimiterCharacter `P.manyTill` P.space
        P.skipSpace
        delim2 <- allowedDelimiterCharacter `P.manyTill` P.try (P.skipSpace >> P.string (T.cons delimiterChange end))
        when (delim1 == (∅) || delim2 == (∅))
          $ fail "Tags must contain more than 0 characters"
        return (delim1, delim2)
      modify $ \oldState -> oldState { sDelimiters = (T.pack delim1, T.pack delim2) }


genParseTagEnd :: T.Text -> MParser DataIdentifier
genParseTagEnd emod = do
  (MustacheState { sDelimiters = ( start, end ) }) <- get

  let nEnd = emod ⊕ end
      disallowed = nub $ nestingSeparator : T.unpack (start ⊕ end)

      parseOne :: P.Parser [T.Text]
      parseOne = do

        one <- P.satisfy (P.notInClass disallowed)
          `P.manyTill`
            (P.try (P.skipSpace >> void (P.string nEnd))
            <|> void (P.char nestingSeparator))

        others <- (P.char nestingSeparator >> parseOne)
                  <|> (const (∅) <$> (P.skipSpace >> P.string nEnd))
        return $ T.pack one : others
  lift $ do P.skipSpace
            (do
              void $ P.char implicitIterator
              P.skipSpace
              void $ P.string nEnd
              return Implicit
              ) <|> (NamedData <$> parseOne)
