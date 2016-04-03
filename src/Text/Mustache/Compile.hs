{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Text.Mustache.Compile
  ( automaticCompile, localAutomaticCompile, TemplateCache, compileTemplateWithCache
  , compileTemplate, cacheFromList, getPartials, getFile, mustache
  ) where


import           Control.Arrow              ((&&&))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Unicode
import           Data.Bool
import           Data.HashMap.Strict        as HM
import           Data.Monoid.Unicode        ((∅))
import           Data.Text                  hiding (concat, find, map, uncons)
import qualified Data.Text.IO               as TIO
import           Language.Haskell.TH        (Q, Exp, Loc, location, loc_filename, loc_start)
import qualified Language.Haskell.TH.Syntax as THS
import           Language.Haskell.TH.Quote  (QuasiQuoter(QuasiQuoter), quoteExp)
import           Prelude.Unicode
import           System.Directory
import           System.FilePath
import           Text.Mustache.Parser
import           Text.Mustache.Types
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
automaticCompile ∷ [FilePath] → FilePath → IO (Either ParseError Template)
automaticCompile searchSpace = compileTemplateWithCache searchSpace (∅)


-- | Compile the template with the search space set to only the current directory
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
    compile' ∷ FilePath
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
          compiled@(Template { ast = mSTree }) ←
            lift $ hoistEither $ compileTemplate name' rawSource

          foldM
            (\st@(Template { partials = p }) partialName → do
              nt ← compile' partialName
              modify (HM.insert partialName nt)
              return (st { partials = HM.insert partialName nt p })
            )
            compiled
            (getPartials mSTree)


-- | Flatten a list of Templates into a single 'TemplateChache'
cacheFromList ∷ [Template] → TemplateCache
cacheFromList = flattenPartials ∘ fromList ∘ fmap (name &&& id)


-- | Compiles a 'Template' directly from 'Text' without checking for missing partials.
-- the result will be a 'Template' with an empty 'partials' cache.
compileTemplate ∷ String → Text → Either ParseError Template
compileTemplate name' = fmap (flip (Template name') (∅)) ∘ parse name'


{-|
  Find the names of all included partials in a mustache STree.

  Same as @join . fmap getPartials'@
-}
getPartials ∷ STree → [FilePath]
getPartials = join ∘ fmap getPartials'


{-|
  Find partials in a single Node
-}
getPartials' ∷ Node Text → [FilePath]
getPartials' (Partial _ p) = return p
getPartials' (Section _ n) = getPartials n
getPartials' (InvertedSection _ n) = getPartials n
getPartials' _                     = (∅)


flattenPartials ∷ TemplateCache → TemplateCache
flattenPartials m = foldrWithKey (insertWith (\_ b -> b)) m m


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


-- |
-- Compile a mustache 'Template' at compile time. Usage:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Text.Mustache.Compile (mustache)
-- >
-- > foo :: Template
-- > foo = [mustache|This is my inline {{ template }} created at compile time|]
--
-- Partials are not supported in compile time templates.

mustache ∷ QuasiQuoter
mustache = QuasiQuoter {quoteExp = \unprocessedTemplate -> do
  l <- location
  compileTemplateTH (fileAndLine l) unprocessedTemplate }

fileAndLine ∷ Loc -> String
fileAndLine loc = loc_filename loc ++ ":" ++ (show ∘ fst ∘ loc_start $ loc)

compileTemplateTH ∷ String -> String -> Q Exp
compileTemplateTH filename unprocessed =
  either (fail ∘ ("Parse error in mustache template: " ++) ∘ show) THS.lift $ compileTemplate filename (pack unprocessed)


-- ERRORS

fileNotFound ∷ FilePath → ParseError
fileNotFound fp = newErrorMessage (Message $ printf "Template file '%s' not found" fp) (initialPos fp)
