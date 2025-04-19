{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Mustache.Compile
  ( automaticCompile
  , localAutomaticCompile
  , TemplateCache
  , compileTemplateWithCache
  , compileTemplate
  , cacheFromList
  , getPartials
  , mustache
  , embedTemplate
  , embedSingleTemplate
  ) where


import           Control.Arrow ( (&&&) )
import           Control.Monad ( (<=<), filterM, foldM )
import           Control.Monad.Except
                   ( ExceptT (..), MonadError (..), runExceptT )
import           Control.Monad.State
                   ( MonadState (..), MonadTrans (..), StateT, evalStateT
                   , modify
                   )
import           Data.Bool ( bool )
import qualified Data.HashMap.Strict as HM
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Language.Haskell.TH
                   ( Exp, Loc, Q, loc_filename, loc_start, location )
import           Language.Haskell.TH.Quote
                   ( QuasiQuoter (..), quoteExp )
import qualified Language.Haskell.TH.Syntax as THS
import           System.Directory ( doesFileExist, makeAbsolute )
import           System.FilePath ( (</>) )
import           Text.Mustache.Parser ( parse )
import           Text.Mustache.Types
                   ( STree, Template (..), TemplateCache, Node (..) )
import           Text.Parsec.Error ( Message (..), ParseError, newErrorMessage )
import           Text.Parsec.Pos ( initialPos )
import           Text.Printf ( printf )


{-|
  Compiles a mustache template provided by name including the mentioned partials.

  The same can be done manually using 'getFile', 'mustacheParser' and 'getPartials'.

  This function also ensures each partial is only compiled once even though it may
  be included by other partials including itself.

  A reference to the included template will be found in each including templates
  'partials' section.
-}
automaticCompile :: [FilePath] -> FilePath -> IO (Either ParseError Template)
automaticCompile searchSpace = compileTemplateWithCache searchSpace mempty


-- | Compile the template with the search space set to only the current directory
localAutomaticCompile :: FilePath -> IO (Either ParseError Template)
localAutomaticCompile = automaticCompile ["."]


{-|
  Compile a mustache template providing a list of precompiled templates that do
  not have to be recompiled.
-}
compileTemplateWithCache :: [FilePath]
                         -> TemplateCache
                         -> FilePath
                         -> IO (Either ParseError Template)
compileTemplateWithCache searchSpace templates initName =
  runExceptT $ evalStateT (compile' initName) $ flattenPartials templates
  where
    compile' :: FilePath
             -> StateT
                (HM.HashMap String Template)
                (ExceptT ParseError IO)
                Template
    compile' name' = do
      templates' <- get
      case HM.lookup name' templates' of
        Just template -> pure template
        Nothing -> do
          rawSource <- lift $ getFile searchSpace name'
          compiled@(Template { ast = mSTree }) <-
            lift $ ExceptT . pure $ compileTemplate name' rawSource

          foldM
            (\st@(Template { partials = p }) partialName -> do
              nt <- compile' partialName
              modify (HM.insert partialName nt)
              pure (st { partials = HM.insert partialName nt p })
            )
            compiled
            (getPartials mSTree)


-- | Flatten a list of Templates into a single 'TemplateCache'
cacheFromList :: [Template] -> TemplateCache
cacheFromList = flattenPartials . HM.fromList . fmap (name &&& id)


-- | Compiles a 'Template' directly from 'Text' without checking for missing partials.
-- the result will be a 'Template' with an empty 'partials' cache.
compileTemplate :: String -> Text -> Either ParseError Template
compileTemplate name' = fmap (flip (Template name') mempty) . parse name'


{-|
  Find the names of all included partials in a mustache STree.

  Same as @join . fmap getPartials'@
-}
getPartials :: STree -> [FilePath]
getPartials = (getPartials' =<<)


{-|
  Find partials in a single Node
-}
getPartials' :: Node Text -> [FilePath]
getPartials' (Partial _ p) = pure p
getPartials' (Section _ n) = getPartials n
getPartials' (ExistingSection _ n) = getPartials n
getPartials' (InvertedSection _ n) = getPartials n
getPartials' _                     = mempty


flattenPartials :: TemplateCache -> TemplateCache
flattenPartials m = HM.foldrWithKey (HM.insertWith (\_ b -> b)) m m


{-|
  @getFile searchSpace file@ iteratively searches all directories in
  @searchSpace@ for a @file@ returning it if found or raising an error if none
  of the directories contain the file.

  This trows 'ParseError's to be compatible with the internal Either Monad of
  'compileTemplateWithCache'.
-}
getFile :: [FilePath] -> FilePath -> ExceptT ParseError IO Text
getFile [] fp = throwError $ fileNotFound fp
getFile (templateDir : xs) fp =
  lift (doesFileExist filePath) >>=
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
-- Partials are not supported in the QuasiQuoter
mustache :: QuasiQuoter
mustache = QuasiQuoter {quoteExp = \unprocessedTemplate -> do
  l <- location
  compileTemplateTH (fileAndLine l) unprocessedTemplate }


-- |
-- Compile a mustache 'Template' at compile time providing a search space for
-- any partials. Usage:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Text.Mustache.Compile (embedTemplate)
-- >
-- > foo :: Template
-- > foo = $(embedTemplate ["dir", "dir/partials"] "file.mustache")
--
embedTemplate :: [FilePath] -> FilePath -> Q Exp
embedTemplate searchSpace filename = do
  template <- either (fail . ("Parse error in mustache template: " ++) . show) pure =<< THS.runIO (automaticCompile searchSpace filename)
  let possiblePaths = do
        fname <- (filename:) . HM.keys . partials $ template
        path <- searchSpace
        pure $ path </> fname
  mapM_ addDependentRelativeFile =<< THS.runIO (filterM doesFileExist possiblePaths)
  THS.lift template


-- |
-- Compile a mustache 'Template' at compile time. Usage:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Text.Mustache.Compile (embedSingleTemplate)
-- >
-- > foo :: Template
-- > foo = $(embedSingleTemplate "dir/file.mustache")
--
-- Partials are not supported in embedSingleTemplate
embedSingleTemplate :: FilePath -> Q Exp
embedSingleTemplate filePath = do
  addDependentRelativeFile filePath
  compileTemplateTH filePath =<< THS.runIO (readFile filePath)


fileAndLine :: Loc -> String
fileAndLine loc = loc_filename loc ++ ":" ++ (show . fst . loc_start $ loc)


compileTemplateTH :: String -> String -> Q Exp
compileTemplateTH filename unprocessed =
  either
    (fail . ("Parse error in mustache template: " ++) . show)
    THS.lift
    (compileTemplate filename (T.pack unprocessed))


addDependentRelativeFile :: FilePath -> Q ()
addDependentRelativeFile = THS.qAddDependentFile <=< THS.runIO . makeAbsolute


-- ERRORS
fileNotFound :: FilePath -> ParseError
fileNotFound fp = newErrorMessage
  (Message $ printf "Template file '%s' not found" fp)
  (initialPos fp)
