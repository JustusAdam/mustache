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
import           Control.Applicative    ((<|>), (<$>))
import           Data.Foldable          (fold, find)
import           Data.HashMap.Strict    as HM hiding (map)
import           Data.Monoid            (mempty, (<>))
import           Data.Text              hiding (concat, find, map, uncons)
import qualified Data.Text              as T
import           Data.Traversable       (traverse)
import qualified Data.Vector            as V
import           Text.HTML.TagSoup      (escapeHTML)
import           Text.Mustache.Internal
import           Text.Mustache.Types
import           Text.Printf
import Data.Scientific (floatingOrInteger)


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.

  Equivalent to @substituteValue . toMustache@.
-}
substitute ∷ ToMustache j ⇒ MustacheTemplate → j → Either String Text
substitute t = substituteValue t . toMustache


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.
-}
substituteValue ∷ MustacheTemplate → Value → Either String Text
substituteValue (MustacheTemplate { ast = cAst, partials = cPartials }) dataStruct =
  joinSubstituted (substitute' (Context mempty dataStruct)) cAst
  where
    joinSubstituted f = fmap fold . traverse f

    -- Main substitution function
    substitute' ∷ Context Value → MustacheNode Text → Either String Text

    -- subtituting text
    substitute' _ (MustacheText t) = return t

    -- substituting a whole section (entails a focus shift)
    substitute' context@(Context parents focus) (MustacheSection secName secAST) =
      case search context secName of
        Just arr@(Array arrCont) →
          if V.null arrCont
            then return mempty
            else flip joinSubstituted arrCont $ \focus' →
              let
                newContext = Context (arr:focus:parents) focus'
              in
                joinSubstituted (substitute' newContext) secAST
        Just (Bool b) | not b → return mempty
        Just (Lambda l) → l context secAST >>= joinSubstituted (substitute' context)
        Just focus' →
          let
            newContext = Context (focus:parents) focus'
          in
            joinSubstituted (substitute' newContext) secAST
        Nothing → return mempty

    -- substituting an inverted section
    substitute' context (MustacheInvertedSection invSecName invSecAST) =
      case search context invSecName of
        Just (Bool False) → contents
        Just (Array a) | V.null a → contents
        Nothing → contents
        _ → return mempty
      where
        contents = joinSubstituted (substitute' context) invSecAST

    -- substituting a variable
    substitute' context (MustacheVariable escaped varName) =
      return $ maybe
        mempty
        (if escaped then escapeHTML else id)
        $ toString <$> search context varName

    -- substituting a partial
    substitute' context (MustachePartial pName) =
      maybe
        (return "")
        (joinSubstituted (substitute' context) . ast)
        $ find ((== pName) . name) cPartials
    substitute' (Context _ val) MustacheImplicitIterator = return $ toString val


search ∷ Context Value → [T.Text] → Maybe Value
search _ [] = Nothing
search (Context parents focus) val@(x:xs) =
  (
    ( case focus of
      (Object o) → HM.lookup x o
      _ → Nothing
    )
    <|> (do
          (newFocus, newParents) <- uncons parents
          search (Context newParents newFocus) val
        )
  )
    >>= innerSearch xs



innerSearch ∷ [T.Text] → Value → Maybe Value
innerSearch [] v = Just v
innerSearch (y:ys) (Object o) = HM.lookup y o >>= innerSearch ys
innerSearch _ _ = Nothing


toString ∷ Value → Text
toString (String t) = t
toString (Number n) = either (pack . show) (pack . show) (floatingOrInteger n ∷ Either Double Integer)
toString e = pack $ show e
