{-# LANGUAGE LambdaCase #-}
module Text.Mustache
  ( substitute
  , compileTemplate
  , compileTemplateWithCache
  , MustacheTemplate(..)
  , getFile
  , getPartials
  , getPartials'
  ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Bool
import           Data.Foldable
import           Data.HashMap.Strict        as HM hiding (map)
import           Data.List
import           Data.Monoid
import           Data.Text                  hiding (concat, find, map, uncons)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import qualified Data.Vector                as V
import           System.Directory
import           System.FilePath
import           Text.HTML.TagSoup          (escapeHTML)
import           Text.Mustache.AST
import           Text.Mustache.Parser
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Printf


{-|
  A compiled Template with metadata.
-}
data MustacheTemplate = MustacheTemplate { name     :: String
                                         , ast      :: MustacheAST
                                         , partials :: [MustacheTemplate]
                                         } deriving (Show)


data Context a = Context [a] a


{-|
  Compiles a mustache template provided by name including the mentioned partials.

  The same can be done manually using 'getFile', 'mustacheParser' and 'getPartials'.

  This function also ensures each partial is only compiled once even though it may
  be included by other partials including itself.

  A reference to the included template will be found in each including templates
  'partials' section.
-}
compileTemplate :: [FilePath] -> FilePath -> IO (Either ParseError MustacheTemplate)
compileTemplate searchSpace = compileTemplateWithCache searchSpace mempty


{-|
  Compile a mustache template providing a list of precompiled templates that do
  not have to be recompiled.
-}
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
            (getPartials compiled)


{-|
  Find the names of all included partials in a mustache AST.

  Same as @join . fmap getPartials'@
-}
getPartials :: MustacheAST -> [FilePath]
getPartials = join . fmap getPartials'


{-|
  Find partials in a single MustacheNode
-}
getPartials' :: MustacheNode Text -> [FilePath]
getPartials' (MustachePartial p) = return p
getPartials' (MustacheSection _ n) = getPartials n
getPartials' _ = mempty


{-|
  @getFile searchSpace file@ iteratively searches all directories in
  @searchSpace@ for a @file@ returning it if found or raising an error if none
  of the directories contain the file.

  This trows 'ParseError's to be compatible with the internal Either Monad of
  'compileTemplateWithCache'.
-}
getFile :: [FilePath] -> FilePath -> EitherT ParseError IO Text
getFile [] fp = throwError $ fileNotFound fp
getFile (templateDir : xs) fp =
  lift (doesFileExist filePath) >>=
    bool
      (getFile xs fp)
      (lift $ TIO.readFile filePath)
  where
    filePath = templateDir </> fp


{-|
  Substitutes all mustache defined tokens (or tags) for values found in the
  provided data structure.
-}
substitute :: ToJSON j => MustacheTemplate -> j -> Either String Text
substitute t = substituteValue t . toJSON


{-|
  Same as @substituteValue template . toJSON@
-}
substituteValue :: MustacheTemplate -> Value -> Either String Text
substituteValue (MustacheTemplate { ast = cAst, partials = cPartials }) dataStruct =
  joinSubstituted (substitute' (Context mempty dataStruct)) cAst
  where
    joinSubstituted f = fmap fold . traverse f

    -- Main substitution function
    substitute' :: Context Value -> MustacheNode Text -> Either String Text

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


search :: Context Value -> T.Text -> Maybe Value
search (Context parents focus) val =
  ($ uncons parents >>=
    flip search val . uncurry (flip Context))
    (case focus of
      Object o -> (HM.lookup val o <|>)
      _ -> id)


toString :: Value -> Text
toString (String t) = t
toString e = pack $ show e


-- ERRORS

fileNotFound :: FilePath -> ParseError
fileNotFound fp = newErrorMessage (Message $ printf "Template file '%s' not found" fp) (initialPos fp)
