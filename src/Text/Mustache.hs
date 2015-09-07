{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.Mustache where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Foldable
import           Data.HashMap.Strict        as HM
import           Data.List
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Traversable
import qualified Data.Vector                as V
import           Debug.Trace
import           Text.HTML.TagSoup          (escapeHTML)
import           Text.Mustache.AST
import           Text.Mustache.Parser
import           Text.Printf


data MustacheTemplate = MustacheTemplate { name     :: String
                                         , ast      :: MustacheAST
                                         , partials :: [MustacheTemplate]
                                         } deriving (Show)


data Context a = Context [a] a


compileTemplate :: FilePath -> IO (Either ParseError MustacheTemplate)
compileTemplate = compileTemplateWithCache mempty


compileTemplateWithCache :: [MustacheTemplate] -> FilePath -> IO (Either ParseError MustacheTemplate)
compileTemplateWithCache cache path = runEitherT $ compile' cache path
  where
    compile' templates name' =
      case find ((== name') . name) templates of
        Just template -> return template
        Nothing -> do
          rawSource <- lift $ readFile name'
          compiled <- hoistEither $ mustacheParser name' rawSource

          foldM
            (\st@(MustacheTemplate { partials = p }) partialName ->
              compile' (p <> templates) partialName >>=
                \nt -> return (st { partials = nt : p })
            )
            MustacheTemplate { name = name', ast = compiled, partials = mempty }
            (join $ fmap getPartials compiled)


getPartials :: MustacheNode String -> [FilePath]
getPartials (MustachePartial p) = return p
getPartials (MustacheSection _ n) = join $ fmap getPartials n
getPartials _ = mempty


substitute :: ToJSON j => MustacheTemplate -> j -> Either String String
substitute (MustacheTemplate { name = tname, ast, partials }) dataStruct =
  joinSub (substitute' (Context mempty (toJSON dataStruct))) ast
  where
    joinSub f = fmap concat . traverse f

    -- Main substitution function
    substitute' :: Context Value -> MustacheNode String -> Either String String

    -- subtituting text
    substitute' _ (MustacheText t) = return t

    -- substituting a whole section (entails a focus shift)
    substitute' c@(Context parents focus) (MustacheSection name ast) =
      case search c (T.pack name) of
        Just focus'@(Object o) ->
          let
            newContext = Context (focus:parents) focus'
          in
            joinSub (substitute' newContext) ast
        Just arr@(Array a) | not (V.null a) -> fmap concat $ for a $ \case
          focus'@(Object _) ->
            let
              newContext = Context (arr:focus:parents) focus'
            in
              joinSub (substitute' newContext) ast
          _ -> return mempty
        _ -> return mempty

    -- substituting an inverted section
    substitute' context (MustacheInvertedSection name ast) =
      case search context (T.pack name) of
        Just (Bool False) -> contents
        Just (Array a) | V.null a -> contents
        Nothing -> contents
        _ -> return mempty
      where
        contents = joinSub (substitute' context) ast

    -- substituting a variable
    substitute' context (MustacheVariable escaped name) =
      return $ maybe
        mempty
        (if escaped then escapeHTML else id)
        $ toString <$> search context (T.pack name)

    -- substituting a partial
    substitute' context (MustachePartial name') =
      case find ((== name') . name) partials of
        Just (MustacheTemplate { ast }) -> joinSub (substitute' context) ast
        Nothing -> Left $ printf "Could not find partial '%s'" name'


search (Context parents focus@(Object o)) val =
  HM.lookup val o <|> (uncons parents >>= flip search val . uncurry (flip Context))
search (Context parents focus) val = uncons parents >>= flip search val . uncurry (flip Context)


toString :: Value -> String
toString (String t) = T.unpack t
toString e = show e
