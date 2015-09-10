{-# LANGUAGE UnicodeSyntax #-}
module Text.Mustache.Compile where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Data.Bool
import           Data.Foldable              (fold)
import           Data.List
import           Data.Monoid
import           Data.Text                  hiding (concat, find, map, uncons)
import qualified Data.Text.IO               as TIO
import           System.Directory
import           System.FilePath
import           Text.Mustache.Types
import           Text.Mustache.Parser
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Printf


{-|
  Compiles a mustache template provided by name including the mentioned partials.

  The same can be done manually using 'getFile', 'mustacheParser' and 'getPartials'.

  This function also ensures each partial is only compiled once even though it may
  be included by other partials including itself.

  A reference to the included template will be found in each including templates
  'partials' section.
-}
compileTemplate ∷ [FilePath] → FilePath → IO (Either ParseError MustacheTemplate)
compileTemplate searchSpace = compileTemplateWithCache searchSpace mempty


{-|
  Compile a mustache template providing a list of precompiled templates that do
  not have to be recompiled.
-}
compileTemplateWithCache ∷ [FilePath] → [MustacheTemplate] → FilePath → IO (Either ParseError MustacheTemplate)
compileTemplateWithCache searchSpace = (runEitherT .) . compile'
  where
    compile' templates name' =
      case find ((== name') . name) templates of
        Just template → return template
        Nothing → do
          rawSource ← getFile searchSpace name'
          compiled@(MustacheTemplate { ast = mast }) ←
            hoistEither $ parseTemplate name' rawSource

          foldM
            (\st@(MustacheTemplate { partials = p }) partialName →
              compile' (p <> templates) partialName >>=
                \nt → return (st { partials = nt : p })
            )
            compiled
            (getPartials mast)


parseTemplate ∷ String → Text → Either ParseError MustacheTemplate
parseTemplate name' = fmap (flip (MustacheTemplate name') mempty) . parse name'


{-|
  Find the names of all included partials in a mustache AST.

  Same as @join . fmap getPartials'@
-}
getPartials ∷ MustacheAST → [FilePath]
getPartials = join . fmap getPartials'


{-|
  Find partials in a single MustacheNode
-}
getPartials' ∷ MustacheNode Text → [FilePath]
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
getFile ∷ [FilePath] → FilePath → EitherT ParseError IO Text
getFile [] fp = throwError $ fileNotFound fp
getFile (templateDir : xs) fp =
  lift (doesFileExist filePath) >>=
    bool
      (getFile xs fp)
      (lift $ TIO.readFile filePath)
  where
    filePath = templateDir </> fp


-- ERRORS

fileNotFound ∷ FilePath → ParseError
fileNotFound fp = newErrorMessage (Message $ printf "Template file '%s' not found" fp) (initialPos fp)
