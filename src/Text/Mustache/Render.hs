{-|
Module      : $Header$
Description : Functions for rendering mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Text.Mustache.Render
  (
    -- * Substitution
    substitute
  , substituteValue
    -- * Checked substitution
  , checkedSubstitute
  , checkedSubstituteValue
  , SubstitutionError (..)
    -- * Working with Context
  , Context (..)
  , search
  , innerSearch
  , SubM
  , substituteNode
  , substituteAST
  , catchSubstitute
    -- * Util
  , toString
  ) where


import           Control.Arrow ( first, second )
import           Control.Monad ( (>=>) )
import           Control.Monad.Reader ( MonadReader (..), asks )
import           Control.Monad.Writer ( MonadWriter (..), censor )
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid ( (<>) )
#endif
import           Data.Foldable ( for_ )
import qualified Data.HashMap.Strict as HM
import           Data.Maybe ( fromMaybe )
import           Data.Scientific ( floatingOrInteger )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import           Text.Mustache.Internal ( escapeXMLText, uncons )
import           Text.Mustache.Internal.Types
                   ( SubM (..), SubstitutionError (..), innerSearch, runSubM
                   , search, shiftContext, tellError, tellSuccess
                   )
import           Text.Mustache.Types
                   ( Context (..), DataIdentifier (..), Node (..), STree
                   , Template (..), ToMustache (..), Value (..), askContext
                   )


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
  second T.concat $ runSubM
    (substituteAST (ast template))
    (Context mempty dataStruct)
    (partials template)


-- | Catch the results of running the inner substitution.
catchSubstitute :: SubM a -> SubM (a, Text)
catchSubstitute =
  fmap (second (T.concat . snd)) . SubM . hideResults . listen . runSubM'
  where
    hideResults = censor (\(errs, _) -> (errs, []))


-- | Substitute an entire 'STree' rather than just a single 'Node'
substituteAST :: STree -> SubM ()
substituteAST = mapM_ substituteNode


-- | Main substitution function
substituteNode :: Node Text -> SubM ()

-- subtituting text
substituteNode (TextBlock t) = tellSuccess t

-- substituting a whole section (entails a focus shift)
substituteNode (Section Implicit secSTree) =
  asks fst >>= \case
    Context parents focus@(Array a)
      | V.null a  -> pure ()
      | otherwise -> for_ a $ \focus' ->
        let newContext = Context (focus:parents) focus'
        in shiftContext newContext $ substituteAST secSTree
    Context _ (Object _) -> substituteAST secSTree
    Context _ v -> tellError $ InvalidImplicitSectionContextType $ showValueType v

substituteNode (Section (NamedData secName) secSTree) =
  search secName >>= \case
    Just arr@(Array arrCont) ->
      if V.null arrCont
        then pure ()
        else do
          Context parents focus <- asks fst
          for_ arrCont $ \focus' ->
            let newContext = Context (arr:focus:parents) focus'
            in shiftContext newContext $ substituteAST secSTree
    Just (Bool False) -> pure ()
    Just Null         -> pure ()
    Just (Lambda l)   -> substituteAST =<< l secSTree
    Just focus'       -> do
      Context parents focus <- asks fst
      let newContext = Context (focus:parents) focus'
      shiftContext newContext $ substituteAST secSTree
    Nothing -> tellError $ SectionTargetNotFound secName

-- substituting an inverted section
substituteNode (InvertedSection  Implicit _) = tellError InvertedImplicitSection
substituteNode (InvertedSection (NamedData secName) invSecSTree) =
  search secName >>= \case
    Just (Bool False) -> contents
    Just (Array a)    | V.null a -> contents
    Just Null         -> contents
    Nothing           -> contents
    _                 -> pure ()
  where
    contents = mapM_ substituteNode invSecSTree

-- substituting a variable
substituteNode (Variable _ Implicit) = asks (ctxtFocus . fst) >>= toString >>= tellSuccess
substituteNode (Variable escaped (NamedData varName)) =
  maybe
    (tellError $ VariableNotFound varName)
    (toString >=> tellSuccess . (if escaped then escapeXMLText else id))
    =<< search varName

-- substituting a partial
substituteNode (Partial indent pName) = do
  cPartials <- asks snd
  case HM.lookup pName cPartials of
    Nothing -> tellError $ PartialNotFound pName
    Just t ->
      let ast' = handleIndent indent $ ast t
      in local (second (partials t `HM.union`)) $ substituteAST ast'


showValueType :: Value -> String
showValueType Null       = "Null"
showValueType (Object _) = "Object"
showValueType (Array _)  = "Array"
showValueType (String _) = "String"
showValueType (Lambda _) = "Lambda"
showValueType (Number _) = "Number"
showValueType (Bool _)   = "Bool"


handleIndent :: Maybe Text -> STree -> STree
handleIndent Nothing ast' = ast'
handleIndent (Just indentation) ast' = preface <> content
  where
    preface = [TextBlock indentation | not (T.null indentation)]
    content = if T.null indentation
      then ast'
      else reverse $ maybe [] (uncurry (:) . first dropper) (uncons (reverse fullIndented))
      where
        fullIndented = fmap (indentBy indentation) ast'
        dropper (TextBlock t) = TextBlock $
          if ("\n" <> indentation) `T.isSuffixOf` t
            then fromMaybe t $ T.stripSuffix indentation t
            else t
        dropper a = a


indentBy :: Text -> Node Text -> Node Text
indentBy indent p@(Partial (Just indent') name')
  | T.null indent = p
  | otherwise = Partial (Just (indent <> indent')) name'
indentBy indent (Partial Nothing name') = Partial (Just indent) name'
indentBy indent (TextBlock t) = TextBlock $ T.replace "\n" ("\n" <> indent) t
indentBy _ a = a


-- | Converts values to Text as required by the mustache standard
toString :: Value -> SubM Text
toString (String t) = pure t
toString (Number n) = pure $ either
  (T.pack . show)
  (T.pack . show)
  (floatingOrInteger n :: Either Double Integer)
toString (Lambda l) = do
  ((), res) <- catchSubstitute $ substituteAST =<< l []
  pure res
toString e          = do
  tellError $ DirectlyRenderedValue e
  pure $ T.pack $ show e


instance ToMustache (Context Value -> STree -> STree) where
  toMustache f = Lambda $ (<$> askContext) . flip f

instance ToMustache (Context Value -> STree -> Text) where
  toMustache = lambdaHelper id

instance ToMustache (Context Value -> STree -> LT.Text) where
  toMustache = lambdaHelper LT.toStrict

instance ToMustache (Context Value -> STree -> String) where
  toMustache = lambdaHelper T.pack

lambdaHelper :: (r -> Text) -> (Context Value -> STree -> r) -> Value
lambdaHelper conv f = Lambda $ (<$> askContext) . wrapper
  where
    wrapper ::  STree -> Context Value -> STree
    wrapper lSTree c = [TextBlock $ conv $ f c lSTree]

instance ToMustache (STree -> SubM Text) where
  toMustache f = Lambda (fmap (pure . TextBlock) . f)

instance ToMustache (Text -> Text) where
  toMustache f = Lambda $ \tree -> do
    (_, res) <- catchSubstitute $ substituteAST tree
    pure [TextBlock $ f res]
