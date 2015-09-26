{-|
Module      : $Header$
Description : Functions for rendering mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Text.Mustache.Render
  (
  -- * Substitution
    substitute, substituteValue
  -- * Working with Context
  , Context(..), search, innerSearch
  -- * Util
  , toString
  ) where

--
import           Control.Applicative    ((<$>), (<|>))
import           Control.Monad.Unicode
import           Data.Foldable          (find, fold)
import           Data.HashMap.Strict    as HM hiding (map)
import           Data.Monoid            (mempty, (<>))
import           Data.Monoid.Unicode
import           Data.Scientific        (floatingOrInteger)
import           Data.Text              hiding (concat, find, map, uncons)
import qualified Data.Text              as T
import           Data.Traversable       (traverse)
import qualified Data.Vector            as V
import           Prelude.Unicode
import           Text.HTML.TagSoup      (escapeHTML)
import           Text.Mustache.Internal
import           Text.Mustache.Types


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.

  Equivalent to @substituteValue . toMustache@.
-}
substitute ∷ ToMustache j ⇒ Template → j → Text
substitute t = substituteValue t ∘ toMustache


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.
-}
substituteValue ∷ Template → Value → Text
substituteValue (Template { ast = cAst, partials = cPartials }) dataStruct =
  joinSubstituted (substitute' (Context mempty dataStruct)) cAst
  where
    joinSubstituted f = fold ∘ fmap f

    -- Main substitution function
    substitute' ∷ Context Value → Node Text → Text

    -- subtituting text
    substitute' _ (TextBlock t) = t

    -- substituting a whole section (entails a focus shift)
    substitute' (Context parents focus@(Array a)) (Section Implicit secAST)
      | V.null a  = (∅)
      | otherwise = flip joinSubstituted a $ \focus' →
        let
          newContext = Context (focus:parents) focus'
        in
          joinSubstituted (substitute' newContext) secAST
    substitute' context@(Context _ (Object _)) (Section Implicit secAST) =
      joinSubstituted (substitute' context) secAST
    substitute' _ (Section Implicit _) = (∅)
    substitute' context@(Context parents focus) (Section (NamedData secName) secAST) =
      case search context secName of
        Just arr@(Array arrCont) →
          if V.null arrCont
            then (∅)
            else flip joinSubstituted arrCont $ \focus' →
              let
                newContext = Context (arr:focus:parents) focus'
              in
                joinSubstituted (substitute' newContext) secAST
        Just (Bool b) | not b → (∅)
        Just (Lambda l) → joinSubstituted (substitute' context) (l context secAST)
        Just focus' →
          let
            newContext = Context (focus:parents) focus'
          in
            joinSubstituted (substitute' newContext) secAST
        Nothing → (∅)

    -- substituting an inverted section
    substitute' _       (InvertedSection  Implicit           _        ) = (∅)
    substitute' context (InvertedSection (NamedData secName) invSecAST) =
      case search context secName of
        Just (Bool False)         → contents
        Just (Array a) | V.null a → contents
        Nothing                   → contents
        _                         → (∅)
      where
        contents = joinSubstituted (substitute' context) invSecAST

    -- substituting a variable
    substitute' (Context _ current) (Variable _ Implicit) = toString current
    substitute' context (Variable escaped (NamedData varName)) =
      maybe
        mempty
        (if escaped then escapeHTML else id)
        $ toString <$> search context varName

    -- substituting a partial
    substitute' context (Partial pName) =
      maybe
        mempty
        (joinSubstituted (substitute' context) . ast)
        $ HM.lookup pName cPartials


search ∷ Context Value → [T.Text] → Maybe Value
search _ [] = Nothing
search (Context parents focus) val@(x:xs) =
  (
    ( case focus of
      (Object o) → HM.lookup x o
      _ → Nothing
    )
    <|> ( do
          (newFocus, newParents) ← uncons parents
          search (Context newParents newFocus) val
        )
  )
    ≫= innerSearch xs



innerSearch ∷ [T.Text] → Value → Maybe Value
innerSearch [] v = Just v
innerSearch (y:ys) (Object o) = HM.lookup y o ≫= innerSearch ys
innerSearch _ _ = Nothing


toString ∷ Value → Text
toString (String t) = t
toString (Number n) = either (pack ∘ show) (pack ∘ show) (floatingOrInteger n ∷ Either Double Integer)
toString e          = pack $ show e
