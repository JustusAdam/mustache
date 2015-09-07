{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.Mustache
  ( substitute
  , compileTemplate
  , compileTemplateWithCache
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Bool
import           Data.Foldable
import           Data.HashMap.Strict        as HM
import           Data.List
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Traversable
import qualified Data.Vector                as V
import           Debug.Trace
import           System.Directory
import           System.FilePath
import           Text.HTML.TagSoup          (escapeHTML)
import           Text.Mustache.AST
import           Text.Mustache.Parser
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Printf


data MustacheTemplate = MustacheTemplate { name     :: String
                                         , ast      :: MustacheAST
                                         , partials :: [MustacheTemplate]
                                         } deriving (Show)


data Context a = Context [a] a


compileTemplate :: [FilePath] -> FilePath -> IO (Either ParseError MustacheTemplate)
compileTemplate searchSpace = compileTemplateWithCache searchSpace mempty


compileTemplateWithCache :: [FilePath] -> [MustacheTemplate] -> FilePath -> IO (Either ParseError MustacheTemplate)
compileTemplateWithCache searchSpace = (runEitherT .) . compile'
  where
    compile' templates name' =
      case find ((== name') . name) templates of
        Just template -> return template
        Nothing -> do
          rawSource <- getFile searchSpace name'
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


getFile :: [FilePath] -> FilePath -> EitherT ParseError IO String
getFile [] fp = throwError $ fileNotFound fp
getFile (templateDir : xs) fp =
  lift (doesFileExist filePath) >>=
    bool
      (getFile xs fp)
      (lift $ readFile filePath)
  where
    filePath = templateDir </> fp


substitute :: ToJSON j => MustacheTemplate -> j -> Either String String
substitute (MustacheTemplate { name = tname, ast, partials }) dataStruct =
  joinSubstituted (substitute' (Context mempty (toJSON dataStruct))) ast
  where
    joinSubstituted f = fmap concat . traverse f

    -- Main substitution function
    substitute' :: Context Value -> MustacheNode String -> Either String String

    -- subtituting text
    substitute' _ (MustacheText t) = return t

    -- substituting a whole section (entails a focus shift)
    substitute' context@(Context parents focus) (MustacheSection name ast) =
      case search context (T.pack name) of
        Just arr@(Array a) ->
          if V.null a
            then return mempty
            else flip joinSubstituted a $ \case
              focus'@(Object _) ->
                let
                  newContext = Context (arr:focus:parents) focus'
                in
                  joinSubstituted (substitute' newContext) ast
              _ -> return mempty
        Just (Bool b) | not b -> return mempty
        Just focus' ->
          let
            newContext = Context (focus:parents) focus'
          in
            joinSubstituted (substitute' newContext) ast
        Nothing -> return mempty

    -- substituting an inverted section
    substitute' context (MustacheInvertedSection name ast) =
      case search context (T.pack name) of
        Just (Bool False) -> contents
        Just (Array a) | V.null a -> contents
        Nothing -> contents
        _ -> return mempty
      where
        contents = joinSubstituted (substitute' context) ast

    -- substituting a variable
    substitute' context (MustacheVariable escaped name) =
      return $ maybe
        mempty
        (if escaped then escapeHTML else id)
        $ toString <$> search context (T.pack name)

    -- substituting a partial
    substitute' context (MustachePartial name') =
      case find ((== name') . name) partials of
        Just (MustacheTemplate { ast }) -> joinSubstituted (substitute' context) ast
        Nothing -> Left $ printf "Could not find partial '%s'" name'


search :: Context Value -> T.Text -> Maybe Value
search (Context parents focus@(Object o)) val =
  HM.lookup val o <|> (uncons parents >>=
    flip search val . uncurry (flip Context))
search (Context parents focus) val =
  uncons parents >>=
    flip search val . uncurry (flip Context)


toString :: Value -> String
toString (String t) = T.unpack t
toString e = show e


-- ERRORS

fileNotFound fp = newErrorMessage (Message $ printf "Template file '%s' not found" fp) (initialPos fp)
