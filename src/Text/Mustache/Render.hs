{-|
Module      : $Header$
Description : Functions for rendering mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Mustache.Render
  (
  -- * Substitution
    substitute, substituteValue
  -- * Checked substitution
  , checkedSubstitute, checkedSubstituteValue, SubstitutionError(..)
  -- * Working with Context
  , Context(..), search, innerSearch
  -- * Util
  , toString
  ) where


import           Control.Applicative    ((<|>))
import           Control.Arrow          (first, second)
import           Control.Monad

import           Data.Foldable          (for_)
import           Data.HashMap.Strict    as HM hiding (keys, map)
import           Data.Maybe             (fromMaybe)

import           Data.Scientific        (floatingOrInteger)
import           Data.Text              as T (Text, isSuffixOf, pack, replace,
                                              stripSuffix)
import qualified Data.Vector            as V
import           Prelude                hiding (length, lines, unlines)

import           Control.Monad.Writer
import qualified Data.Text              as T
import           Text.Mustache.Internal
import           Text.Mustache.Types


-- | Type of errors we may encounter during substitution.
data SubstitutionError
  = VariableNotFound [Key] -- ^ The template contained a variable for which there was no data counterpart in the current context
  | InvalidImplicitSectionContextType String -- ^ When substituting an implicit section the current context had an unsubstitutable type
  | InvertedImplicitSection -- ^ Inverted implicit sections should never occur
  | SectionTargetNotFound [Key] -- ^ The template contained a section for which there was no data counterpart in the current context
  | PartialNotFound FilePath -- ^ The template contained a partial for which there was no data counterpart in the current context
  | DirectlyRenderedValue Value -- ^ A complex value such as an Object or Array was directly rendered into the template (warning)
  deriving (Show)


type Substitution = Writer ([SubstitutionError], [Text])


tellError :: SubstitutionError -> Substitution ()
tellError e = tell ([e], [])


tellSuccess :: Text -> Substitution ()
tellSuccess s = tell ([], [s])


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.

  Equivalent to @substituteValue . toMustache@.
-}
substitute :: ToMustache k => Template -> k -> Text
substitute t = substituteValue t . toMustache


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure and report any errors and warnings encountered during
  substitution.

  This function always produces results, as in a fully substituted/rendered template,
  it never halts on errors. It simply reports them in the first part of the tuple.
  Sites with errors are usually substituted with empty string.

  The second value in the tuple is a template rendered with errors ignored.
  Therefore if you must enforce that there were no errors during substitution
  you must check that the error list in the first tuple value is empty.

  Equivalent to @checkedSubstituteValue . toMustache@.
-}
checkedSubstitute :: ToMustache k => Template -> k -> ([SubstitutionError], Text)
checkedSubstitute t = checkedSubstituteValue t . toMustache


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.
-}
substituteValue :: Template -> Value -> Text
substituteValue = (snd .) . checkedSubstituteValue


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure and report any errors and warnings encountered during
  substitution.

  This function always produces results, as in a fully substituted/rendered template,
  it never halts on errors. It simply reports them in the first part of the tuple.
  Sites with errors are usually substituted with empty string.

  The second value in the tuple is a template rendered with errors ignored.
  Therefore if you must enforce that there were no errors during substitution
  you must check that the error list in the first tuple value is empty.
-}
checkedSubstituteValue :: Template -> Value -> ([SubstitutionError], Text)
checkedSubstituteValue template dataStruct =
  second T.concat $ execWriter $ substituteASTWithValAndCache (ast template) (partials template) (Context mempty dataStruct)

substituteASTWithValAndCache :: STree -> TemplateCache -> Context Value -> Substitution ()
substituteASTWithValAndCache cAst cPartials ctx =
  mapM_ (substitute' ctx) cAst
  where
    -- Main substitution function
    substitute' :: Context Value -> Node Text -> Substitution ()

    -- subtituting text
    substitute' _ (TextBlock t) = tellSuccess t

    -- substituting a whole section (entails a focus shift)
    substitute' (Context parents focus@(Array a)) (Section Implicit secSTree)
      | V.null a  = return ()
      | otherwise = for_ a $ \focus' ->
        let
          newContext = Context (focus:parents) focus'
        in
          mapM_ (substitute' newContext) secSTree
    substitute' context@(Context _ (Object _)) (Section Implicit secSTree) =
      mapM_ (substitute' context) secSTree
    substitute' (Context _ v) (Section Implicit _) =
      tellError $ InvalidImplicitSectionContextType $ showValueType v
    substitute' context@(Context parents focus) (Section (NamedData secName) secSTree) =
      case search context secName of
        Just arr@(Array arrCont) ->
          if V.null arrCont
            then return ()
            else for_ arrCont $ \focus' ->
              let
                newContext = Context (arr:focus:parents) focus'
              in
                mapM_ (substitute' newContext) secSTree
        Just (Bool False) -> return ()
        Just Null -> return ()
        Just (Lambda l)   -> mapM_ (substitute' context) (l context secSTree)
        Just focus'       ->
          let
            newContext = Context (focus:parents) focus'
          in
            mapM_ (substitute' newContext) secSTree
        Nothing -> tellError $ SectionTargetNotFound secName

    -- substituting an inverted section
    substitute' _       (InvertedSection  Implicit           _        ) = tellError InvertedImplicitSection
    substitute' context (InvertedSection (NamedData secName) invSecSTree) =
      case search context secName of
        Just (Bool False)         -> contents
        Just (Array a) | V.null a -> contents
        Nothing                   -> contents
        _                         -> return ()
      where
        contents = mapM_ (substitute' context) invSecSTree

    -- substituting a variable
    substitute' (Context _ current) (Variable _ Implicit) = toString current >>= tellSuccess
    substitute' context (Variable escaped (NamedData varName)) =
      maybe
        (tellError $ VariableNotFound varName)
        (toString >=> tellSuccess . (if escaped then escapeXMLText else id))
        $ search context varName

    -- substituting a partial
    substitute' context (Partial indent pName) =
      case HM.lookup pName cPartials of
        Nothing -> tellError $ PartialNotFound pName
        Just t ->
          let ast' = handleIndent indent $ ast t
          in substituteASTWithValAndCache ast' (partials t `HM.union` cPartials) context


showValueType :: Value -> String
showValueType Null = "Null"
showValueType (Object _) = "Object"
showValueType (Array _) = "Array"
showValueType (String _) = "String"
showValueType (Lambda _) = "Lambda"
showValueType (Number _) = "Number"
showValueType (Bool _) = "Bool"


handleIndent :: Maybe Text -> STree -> STree
handleIndent Nothing ast' = ast'
handleIndent (Just indentation) ast' = preface <> content
  where
    preface = if T.null indentation then [] else [TextBlock indentation]
    content = if T.null indentation
      then ast'
      else
        let
          fullIndented = fmap (indentBy indentation) ast'
          dropper (TextBlock t) = TextBlock $
            if ("\n" <> indentation) `isSuffixOf` t
              then fromMaybe t $ stripSuffix indentation t
              else t
          dropper a = a
        in
          reverse $ fromMaybe [] (uncurry (:) . first dropper <$> uncons (reverse fullIndented))


-- | Search for a key in the current context.
--
-- The search is conducted inside out mening the current focus
-- is searched first. If the key is not found the outer scopes are recursively
-- searched until the key is found, then 'innerSearch' is called on the result.
search :: Context Value -> [Key] -> Maybe Value
search _ [] = Nothing
search ctx keys@(_:nextKeys) = go ctx keys >>= innerSearch nextKeys
  where
    go _ [] = Nothing
    go (Context parents focus) val@(x:_) = searchCurrentContext <|> searchParentContext
      where
        searchCurrentContext = case focus of
                                  (Object o) -> HM.lookup x o
                                  _          -> Nothing
        searchParentContext = do
          (newFocus, newParents) <- uncons parents
          go (Context newParents newFocus) val

indentBy :: Text -> Node Text -> Node Text
indentBy indent p@(Partial (Just indent') name')
  | T.null indent = p
  | otherwise = Partial (Just (indent <> indent')) name'
indentBy indent (Partial Nothing name') = Partial (Just indent) name'
indentBy indent (TextBlock t) = TextBlock $ replace "\n" ("\n" <> indent) t
indentBy _ a = a


-- | Searches nested scopes navigating inward. Fails if it encunters something
-- other than an object before the key is expended.
innerSearch :: [Key] -> Value -> Maybe Value
innerSearch []     v          = Just v
innerSearch (y:ys) (Object o) = HM.lookup y o >>= innerSearch ys
innerSearch _      _          = Nothing


-- | Converts values to Text as required by the mustache standard
toString :: Value -> Substitution Text
toString (String t) = return t
toString (Number n) = return $ either (pack . show) (pack . show) (floatingOrInteger n :: Either Double Integer)
toString e          = do
  tellError $ DirectlyRenderedValue e
  return $ pack $ show e
