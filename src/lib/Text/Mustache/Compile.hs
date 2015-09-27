{-# LANGUAGE UnicodeSyntax #-}
module Text.Mustache.Compile
  ( automaticCompile, localAutomaticCompile, TemplateCache, compileTemplateWithCache
  , parseTemplate, cacheFromList, getPartials
  ) where


import           Control.Applicative
import           Control.Arrow              ((&&&))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Unicode
import           Data.Bool
import           Data.Foldable              (fold)
import           Data.Function.JAExtra
import           Data.HashMap.Strict        as HM
import           Data.List
import           Data.Monoid
import           Data.Monoid.Unicode
import           Data.Text                  hiding (concat, find, map, uncons)
import qualified Data.Text.IO               as TIO
import           Prelude.Unicode
import           System.Directory
import           System.FilePath
import           Text.Mustache.Parser
import           Text.Mustache.Types
import           Text.Parsec.Error
import           Text.Parsec.Pos
import           Text.Printf


type TemplateCache = HM.HashMap String Template


{-|
  Compiles a mustache template provided by name including the mentioned partials.

  The same can be done manually using 'getFile', 'mustacheParser' and 'getPartials'.

  This function also ensures each partial is only compiled once even though it may
  be included by other partials including itself.

  A reference to the included template will be found in each including templates
  'partials' section.
-}
automaticCompile ∷ [FilePath] → FilePath → IO (Either ParseError Template)
automaticCompile searchSpace = compileTemplateWithCache searchSpace (∅)


localAutomaticCompile ∷ FilePath → IO (Either ParseError Template)
localAutomaticCompile = automaticCompile ["."]


{-|
  Compile a mustache template providing a list of precompiled templates that do
  not have to be recompiled.
-}
compileTemplateWithCache ∷ [FilePath]
                         → TemplateCache
                         → FilePath
                         → IO (Either ParseError Template)
compileTemplateWithCache searchSpace templates initName =
  runEitherT $ evalStateT (compile' initName) $ flattenPartials templates
  where
    compile' :: FilePath
             → StateT
                (HM.HashMap String Template)
                (EitherT ParseError IO)
                Template
    compile' name' = do
      templates' ← get
      case HM.lookup name' templates' of
        Just template → return template
        Nothing → do
          rawSource ← lift $ getFile searchSpace name'
          compiled@(Template { ast = mAST }) ←
            lift $ hoistEither $ parseTemplate name' rawSource

          foldM
            (\st@(Template { partials = p }) partialName → do
              nt ← compile' partialName
              modify (HM.insert partialName nt)
              return (st { partials = HM.insert partialName nt p })
            )
            compiled
            (getPartials mAST)


cacheFromList ∷ [Template] → TemplateCache
cacheFromList = flattenPartials ∘ fromList ∘ fmap (name &&& id)


parseTemplate ∷ String → Text → Either ParseError Template
parseTemplate name' = fmap (flip (Template name') (∅)) ∘ parse name'


{-|
  Find the names of all included partials in a mustache AST.

  Same as @join . fmap getPartials'@
-}
getPartials ∷ AST → [FilePath]
getPartials = join ∘ fmap getPartials'


{-|
  Find partials in a single Node
-}
getPartials' ∷ Node Text → [FilePath]
getPartials' (Partial         p  ) = return p
getPartials' (Section         _ n) = getPartials n
getPartials' (InvertedSection _ n) = getPartials n
getPartials' _                     = (∅)


flattenPartials ∷ TemplateCache → TemplateCache
flattenPartials = stuffWith $ foldrWithKey $ insertWith discard


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
  lift (doesFileExist filePath) ≫=
    bool
      (getFile xs fp)
      (lift $ TIO.readFile filePath)
  where
    filePath = templateDir </> fp


-- ERRORS

fileNotFound ∷ FilePath → ParseError
fileNotFound fp = newErrorMessage (Message $ printf "Template file '%s' not found" fp) (initialPos fp)
