{-# LANGUAGE OverloadedStrings    #-}
module Text.Mustache.Render where

--
import           Control.Applicative
import           Data.Foldable              (fold)
import           Data.HashMap.Strict        as HM hiding (map)
import           Data.List
import           Data.Monoid
import           Data.Text                  hiding (concat, find, map, uncons)
import qualified Data.Text                  as T
import           Data.Traversable           (traverse)
import qualified Data.Vector                as V
import           Text.HTML.TagSoup          (escapeHTML)
import           Text.Mustache.Types
import           Text.Printf


data Context a = Context [a] a


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.
-}
substitute :: ToMustache j => MustacheTemplate -> j -> Either String Text
substitute t = substituteValue t . toMustache


{-|
  Same as @substituteValue template . toJSON@
-}
substituteValue :: MustacheTemplate -> MustacheData -> Either String Text
substituteValue (MustacheTemplate { ast = cAst, partials = cPartials }) dataStruct =
  joinSubstituted (substitute' (Context mempty dataStruct)) cAst
  where
    joinSubstituted f = fmap fold . traverse f

    -- Main substitution function
    substitute' :: Context MustacheData -> MustacheNode Text -> Either String Text

    -- subtituting text
    substitute' _ (MustacheText t) = return t

    -- substituting a whole section (entails a focus shift)
    substitute' context@(Context parents focus) (MustacheSection secName secAST) =
      case search context secName of
        Just arr@(Array arrCont) ->
          if V.null arrCont
            then return mempty
            else flip joinSubstituted arrCont $ \focus' ->
              let
                newContext = Context (arr:focus:parents) focus'
              in
                joinSubstituted (substitute' newContext) secAST
        Just (Bool b) | not b -> return mempty
        Just focus' ->
          let
            newContext = Context (focus:parents) focus'
          in
            joinSubstituted (substitute' newContext) secAST
        Nothing -> return mempty

    -- substituting an inverted section
    substitute' context (MustacheInvertedSection invSecName invSecAST) =
      case search context invSecName of
        Just (Bool False) -> contents
        Just (Array a) | V.null a -> contents
        Nothing -> contents
        _ -> return mempty
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
        (Left $ printf "Could not find partial '%s'" pName)
        (joinSubstituted (substitute' context) . ast)
        $ find ((== pName) . name) cPartials


search :: Context MustacheData -> [T.Text] -> Maybe MustacheData
search _ [] = Nothing
search (Context parents focus) val@(x:xs) =
  (
    ( case focus of
      (Object o) -> HM.lookup x o
      _ -> Nothing
    )
    <|> (do
          (newFocus, newParents) <- uncons parents
          search (Context newParents newFocus) val
        )
  )
    >>= innerSearch xs



innerSearch :: [T.Text] -> MustacheData -> Maybe MustacheData
innerSearch [] v = Just v
innerSearch (y:ys) (Object o) = HM.lookup y o >>= innerSearch ys
innerSearch _ _ = Nothing


toString :: MustacheData -> Text
toString (String t) = t
toString e = pack $ show e
