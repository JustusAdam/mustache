{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative    ((<$>), (<*>))
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy   (toStrict)
import           Data.Foldable          (for_)
import qualified Data.HashMap.Strict    as HM (HashMap, empty,
                                               traverseWithKey)
import           Data.List
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Data.Yaml              as Y (FromJSON, Value (..), decode,
                                              parseJSON, (.!=), (.:), (.:?))
import           Network.Wreq
import           System.FilePath
import           Test.Hspec
import           Text.Mustache


langspecs :: [String]
langspecs =
  [ "https://codeload.github.com/andrewthad/spec/legacy.tar.gz/add_list_context_check"
  , "https://codeload.github.com/mustache/spec/tar.gz/v1.1.3"
  ]


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


getOfficialSpecRelease :: String -> IO [(String, LangSpecFile)]
getOfficialSpecRelease releaseURL  = do
    res <- get releaseURL
    let archive = Tar.read $ GZip.decompress (res ^. responseBody)
    return $ Tar.foldEntries handleEntry [] (error . show) archive
  where
    handleEntry e acc =
      case content of
        Tar.NormalFile f _
          | takeExtension filename `elem` [".yml", ".yaml"]
              && not ("~" `isPrefixOf` takeFileName filename) ->
                (filename, fromMaybe (error $ "Error parsing spec file " ++ filename) $ decode $ toStrict f):acc
        _ -> acc
      where
        filename = Tar.entryPath e
        content = Tar.entryContent e


testOfficialLangSpec :: [(String, LangSpecFile)] -> Spec
testOfficialLangSpec testfiles =
  for_ testfiles $ \(filename, LangSpecFile { tests }) ->
    describe ("File: " ++ takeFileName filename) $
      for_ tests $ \(LangSpecTest { .. }) ->
        it ("Name: " ++ name ++ "  Description: " ++ specDescription) $
          let
            compiled = do
              partials' <- HM.traverseWithKey compileTemplate testPartials
              template' <- compileTemplate name template
              return $ template' { partials = partials' }
          in
            case compiled of
              Left m -> expectationFailure $ show m
              Right tmp ->
                substituteValue tmp (toMustache specData) `shouldBe` expected


main :: IO ()
main =
  void $ do
    specs <- mapM getOfficialSpecRelease langspecs
    hspec $ mapM_ testOfficialLangSpec specs
