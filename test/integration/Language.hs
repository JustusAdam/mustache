{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad
import           Data.Either
import           Data.Foldable        (for_)
import qualified Data.HashMap.Strict  as HM (HashMap, elems, empty, lookup,
                                             traverseWithKey)
import           Data.List
import           Data.Monoid          (mempty, (<>))
import qualified Data.Text            as T
import           Data.Yaml            as Y (FromJSON, Value (..), decodeFile,
                                            parseJSON, (.!=), (.:), (.:?))
import           Debug.Trace          (traceShowId)
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Hspec
import           Text.Mustache
import           Text.Mustache.Parser
import           Text.Mustache.Types

-- langspecDir = "spec-1.1.3"
langspecDir = "andrewthad-spec-786e4ac"
specDir = "specs"
releaseFile = "langspec.tar.gz"
-- releaseURL = "https://codeload.github.com/mustache/spec/tar.gz/v1.1.3"
releaseURL = "https://codeload.github.com/andrewthad/spec/legacy.tar.gz/add_list_context_check"


data LangSpecFile = LangSpecFile
  { overview :: String
  , tests    :: [LangSpecTest]
  }


data LangSpecTest = LangSpecTest
  { name            :: String
  , specDescription :: String
  , specData        :: Y.Value
  , template        :: T.Text
  , expected        :: T.Text
  , testPartials    :: HM.HashMap String T.Text
  }


instance FromJSON LangSpecFile where
  parseJSON (Y.Object o) = LangSpecFile
    <$> o .: "overview"
    <*> o .: "tests"
  parseJSON _ = mzero


instance FromJSON LangSpecTest where
  parseJSON (Y.Object o) = LangSpecTest
    <$> o .: "name"
    <*> o .: "desc"
    <*> o .: "data"
    <*> o .: "template"
    <*> o .: "expected"
    <*> o .:? "partials" .!= HM.empty
  parseJSON _ = mzero


(&) ∷ a → (a → b) → b
(&) = flip ($)


getOfficialSpecRelease ∷ FilePath → IO ()
getOfficialSpecRelease tempdir = do
  currentDirectory ← getCurrentDirectory
  setCurrentDirectory tempdir
  createDirectory langspecDir
  callProcess "curl" [releaseURL, "-o", releaseFile]
  callProcess "tar" ["-xf", releaseFile]
  getDirectoryContents "." >>= print
  setCurrentDirectory currentDirectory


testOfficialLangSpec ∷ FilePath → Spec
testOfficialLangSpec dir = do
  allFiles ← runIO $ getDirectoryContents dir
  let testfiles = allFiles
        & filter ((`elem` [".yml", ".yaml"]) . takeExtension)
      -- Filters the lambda tests for now.
        & filter (not . ("~" `isPrefixOf`) . takeFileName)
  for_ testfiles $ \filename →
    runIO (decodeFile (dir </> filename)) >>= \case
      Nothing -> describe ("File: " <> takeFileName filename) $
        it "loads the data file" $
          expectationFailure "Data file could not be parsed"
      Just (LangSpecFile { tests }) →
        describe ("File: " <> takeFileName filename) $
          for_ tests $ \(LangSpecTest { .. }) →
            it ("Name: " <> name <> "  Description: " <> specDescription) $
              let
                compiled = do
                  partials' <- HM.traverseWithKey compileTemplate testPartials
                  template' <- compileTemplate name template
                  return $ template' { partials = partials' }
              in
                case compiled of
                  Left m → expectationFailure $ show m
                  Right tmp →
                    substituteValue tmp (toMustache specData) `shouldBe` expected


main :: IO ()
main =
  void $
    withSystemTempDirectory
      "mustache-test-resources"
      $ \tempdir → do
        getOfficialSpecRelease tempdir
        hspec $
          testOfficialLangSpec (tempdir </> langspecDir </> specDir)
