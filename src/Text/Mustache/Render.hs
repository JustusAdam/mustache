{-|
Module      : $Header$
Description : Functions for rendering mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Text.Mustache.Render
  (
  -- * Substitution
    substitute, substituteValue
  -- * Checked substitution
  , checkedSubstituteValue, SubstitutionResult(..), SubstitutionError(..)
  -- * Working with Context
  , Context(..), search, innerSearch
  -- * Util
  , toString
  ) where


import           Control.Applicative    ((<$>), (<|>))
import           Control.Arrow          (first)
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Foldable          (fold)
import           Data.HashMap.Strict    as HM hiding (map)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Monoid.Unicode
import           Data.Scientific        (floatingOrInteger)
import           Data.Text              as T (Text, isSuffixOf, null, pack,
                                              replace, stripSuffix)
import qualified Data.Vector            as V
import           Prelude                hiding (length, lines, unlines)
import           Prelude.Unicode
import           Text.Mustache.Internal
import           Text.Mustache.Types


data SubstitutionResult α = SubstitutionResult { subErrors :: [SubstitutionError], subResult :: α } deriving (Show)


instance Functor SubstitutionResult where
  fmap f (SubstitutionResult e r) = SubstitutionResult e (f r)

instance Applicative SubstitutionResult where
  pure = noErrors
  a <*> b = do
    a' <- a
    b' <- b
    pure $ a' b'

instance Monad SubstitutionResult where
  (SubstitutionResult e r) >>= f = SubstitutionResult (e <> e1) r2
    where (SubstitutionResult e1 r2) = f r


data SubstitutionError
  = VariableNotFound [Key] -- ^ The template contained a variable for which there was no data counterpart in the current context
  | InvalidImplicitSectionContextType String -- ^ When substituting an implicit section the current context had an unsubstitutable type
  | InvertedImplicitSection -- ^ Inverted implicit sections should never occur
  | SectionTargetNotFound [Key] -- ^ The template contained a section for which there was no data counterpart in the current context
  | PartialNotFound FilePath -- ^ The template contained a partial for which there was no data counterpart in the current context
  | DirectlyRenderedValue Value -- ^ A complex value such as an Object or Array was directly rendered into the template (warning)
  deriving (Show)


instance Monoid α => Monoid (SubstitutionResult α) where
  mappend (SubstitutionResult e1 r1) (SubstitutionResult e2 r2) = SubstitutionResult (e1 <> e2) (r1 <> r2)
  mempty = SubstitutionResult mempty mempty


noErrors :: α -> SubstitutionResult α
noErrors = SubstitutionResult []


oneError :: Monoid α => SubstitutionError -> SubstitutionResult α
oneError e = SubstitutionResult [e] mempty


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.

  Equivalent to @substituteValue . toMustache@.
-}
substitute ∷ ToMustache κ ⇒ Template → κ → Text
substitute t = substituteValue t ∘ toMustache


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.
-}
substituteValue ∷ Template → Value → Text
substituteValue = (subResult .) . checkedSubstituteValue



checkedSubstituteValue :: Template → Value → SubstitutionResult Text
checkedSubstituteValue (Template { ast = cAst, partials = cPartials }) dataStruct =
  joinSubstituted (substitute' (Context (∅) dataStruct)) cAst
  where
    joinSubstituted f = fold ∘ fmap f

    -- Main substitution function
    substitute' ∷ Context Value → Node Text → SubstitutionResult Text

    -- subtituting text
    substitute' _ (TextBlock t) = noErrors t

    -- substituting a whole section (entails a focus shift)
    substitute' (Context parents focus@(Array a)) (Section Implicit secSTree)
      | V.null a  = (∅)
      | otherwise = flip joinSubstituted a $ \focus' →
        let
          newContext = Context (focus:parents) focus'
        in
          joinSubstituted (substitute' newContext) secSTree
    substitute' context@(Context _ (Object _)) (Section Implicit secSTree) =
      joinSubstituted (substitute' context) secSTree
    substitute' (Context _ v) (Section Implicit _) =
      oneError $ InvalidImplicitSectionContextType $ showValueType v
    substitute' context@(Context parents focus) (Section (NamedData secName) secSTree) =
      case search context secName of
        Just arr@(Array arrCont) →
          if V.null arrCont
            then (∅)
            else flip joinSubstituted arrCont $ \focus' →
              let
                newContext = Context (arr:focus:parents) focus'
              in
                joinSubstituted (substitute' newContext) secSTree
        Just (Bool False) → (∅)
        Just (Lambda l)   → joinSubstituted (substitute' context) (l context secSTree)
        Just focus'       →
          let
            newContext = Context (focus:parents) focus'
          in
            joinSubstituted (substitute' newContext) secSTree
        Nothing → oneError $ SectionTargetNotFound secName

    -- substituting an inverted section
    substitute' _       (InvertedSection  Implicit           _        ) = oneError InvertedImplicitSection
    substitute' context (InvertedSection (NamedData secName) invSecSTree) =
      case search context secName of
        Just (Bool False)         → contents
        Just (Array a) | V.null a → contents
        Nothing                   → contents
        _                         → (∅)
      where
        contents = joinSubstituted (substitute' context) invSecSTree

    -- substituting a variable
    substitute' (Context _ current) (Variable _ Implicit) = toString current
    substitute' context (Variable escaped (NamedData varName)) =
      maybe
        (oneError $ VariableNotFound varName)
        (fmap (if escaped then escapeXMLText else id) . toString)
        $ search context varName

    -- substituting a partial
    substitute' context (Partial indent pName) =
      maybe
        (oneError $ PartialNotFound pName)
        (joinSubstituted (substitute' context) ∘ handleIndent indent ∘ ast)
        $ HM.lookup pName cPartials


showValueType :: Value -> String
showValueType Null = "Null"
showValueType (Object _) = "Object"
showValueType (Array _) = "Array"
showValueType (String _) = "String"
showValueType (Lambda _) = "Lambda"
showValueType (Number _) = "Number"
showValueType (Bool _) = "Bool"


handleIndent ∷ Maybe Text → STree → STree
handleIndent Nothing ast' = ast'
handleIndent (Just indentation) ast' = preface ⊕ content
  where
    preface = if T.null indentation then [] else [TextBlock indentation]
    content = if T.null indentation
      then ast'
      else
        let
          fullIndented = fmap (indentBy indentation) ast'
          dropper (TextBlock t) = TextBlock $
            if ("\n" ⊕ indentation) `isSuffixOf` t
              then fromMaybe t $ stripSuffix indentation t
              else t
          dropper a = a
        in
          reverse $ fromMaybe [] (uncurry (:) ∘ first dropper <$> uncons (reverse fullIndented))


-- | Search for a key in the current context.
--
-- The search is conducted inside out mening the current focus
-- is searched first. If the key is not found the outer scopes are recursively
-- searched until the key is found, then 'innerSearch' is called on the result.
search ∷ Context Value → [Key] → Maybe Value
search _ [] = Nothing
search ctx keys@(_:nextKeys) = go ctx keys ≫= innerSearch nextKeys
  where
  go _ [] = Nothing
  go (Context parents focus) val@(x:_) =
    ( case focus of
      (Object o) → HM.lookup x o
      _          → Nothing
    )
    <|> ( do
          (newFocus, newParents) ← uncons parents
          go (Context newParents newFocus) val
        )

indentBy ∷ Text → Node Text → Node Text
indentBy indent p@(Partial (Just indent') name')
  | T.null indent = p
  | otherwise = Partial (Just (indent ⊕ indent')) name'
indentBy indent (Partial Nothing name') = Partial (Just indent) name'
indentBy indent (TextBlock t) = TextBlock $ replace "\n" ("\n" ⊕ indent) t
indentBy _ a = a


-- | Searches nested scopes navigating inward. Fails if it encunters something
-- other than an object before the key is expended.
innerSearch ∷ [Key] → Value → Maybe Value
innerSearch []     v          = Just v
innerSearch (y:ys) (Object o) = HM.lookup y o ≫= innerSearch ys
innerSearch _      _          = Nothing


-- | Converts values to Text as required by the mustache standard
toString ∷ Value → SubstitutionResult Text
toString (String t) = noErrors t
toString (Number n) = noErrors $ either (pack ∘ show) (pack ∘ show) (floatingOrInteger n ∷ Either Double Integer)
toString e          = SubstitutionResult [DirectlyRenderedValue e] $ pack $ show e
