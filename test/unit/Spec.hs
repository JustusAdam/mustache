{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main where


import           Control.Monad
import           Data.Either
import           Data.Foldable
import           Data.Monoid          (mempty, (<>))
import qualified Data.Text            as T
import           Data.Yaml            as Y (FromJSON, Value (..), decodeFile,
                                            parseJSON, (.:))
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Test.Hspec
import           Text.Mustache
import           Text.Mustache.Parser
import           Text.Mustache.Types
import Data.List


langspecDir = "spec-1.1.3"
specDir = "specs"
releaseFile = "langspec.tar.gz"
releaseURL = "https://codeload.github.com/mustache/spec/tar.gz/v1.1.3"


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
  parseJSON _ = mzero


parserSpec :: Spec
parserSpec =
  describe "mustacheParser" $ do
    let lparse = parse "testsuite"
    let returnedOne = return . return

    let text = "test12356p0--=-34{}jnv,\n"

    it "parses text" $
      lparse text `shouldBe` returnedOne (MustacheText text)

    it "parses a variable" $
      lparse "{{name}}" `shouldBe` returnedOne (MustacheVariable True ["name"])

    it "parses a variable with whitespace" $
      lparse "{{ name  }}" `shouldBe` returnedOne (MustacheVariable True ["name"])

    it "allows '-' in variable names" $
      lparse "{{ name-name }}" `shouldBe`
        returnedOne (MustacheVariable True ["name-name"])

    it "allows '_' in variable names" $
      lparse "{{ name_name }}" `shouldBe`
        returnedOne (MustacheVariable True ["name_name"])

    it "parses a variable unescaped with {{{}}}" $
      lparse "{{{name}}}" `shouldBe` returnedOne (MustacheVariable False ["name"])

    it "parses a variable unescaped with {{{}}} with whitespace" $
      lparse "{{{  name     }}}" `shouldBe`
        returnedOne (MustacheVariable False ["name"])

    it "parses a variable unescaped with &" $
      lparse "{{&name}}" `shouldBe` returnedOne (MustacheVariable False ["name"])

    it "parses a variable unescaped with & with whitespace" $
      lparse "{{&  name  }}" `shouldBe`
        returnedOne (MustacheVariable False ["name"])

    it "parses a partial" $
      lparse "{{>myPartial}}" `shouldBe`
        returnedOne (MustachePartial "myPartial")

    it "parses a partial with whitespace" $
      lparse "{{>  myPartial }}" `shouldBe`
        returnedOne (MustachePartial "myPartial")

    it "parses the an empty section" $
      lparse "{{#section}}{{/section}}" `shouldBe`
        returnedOne (MustacheSection ["section"] mempty)

    it "parses the an empty section with whitespace" $
      lparse "{{#   section }}{{/     section }}" `shouldBe`
        returnedOne (MustacheSection ["section"] mempty)

    it "parses a delimiter change" $
      lparse "{{=<< >>=}}<<var>>{{var}}" `shouldBe`
        return [MustacheVariable True ["var"], MustacheText "{{var}}"]

    it "parses a delimiter change with whitespace" $
      lparse "{{=<<   >>=}}<< var   >>{{var}}" `shouldBe`
        return [MustacheVariable True ["var"], MustacheText "{{var}}"]

    it "parses two subsequent delimiter changes" $
      lparse "{{=((  ))=}}(( var ))((=--  $-=))--#section$---/section$-" `shouldBe`
        return [MustacheVariable True ["var"], MustacheSection ["section"] []]

    it "propagates a delimiter change from a nested scope" $
      lparse "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldBe`
        return [MustacheSection ["section"] [], MustacheVariable True ["var"]]

    it "fails if the tag contains illegal characters" $
      lparse "{{#&}}" `shouldSatisfy` isLeft

    it "parses a nested variable" $
      lparse "{{ name.val }}" `shouldBe` returnedOne (MustacheVariable True ["name", "val"])

    it "parses a variable containing whitespace" $
      lparse "{{ val space }}" `shouldBe` returnedOne (MustacheVariable True ["val space"])


substituteSpec :: Spec
substituteSpec =
  describe "substitute" $ do

    let toTemplate ast' = MustacheTemplate "testsuite" ast' []

    it "substitutes a html escaped value for a variable" $
      substitute
        (toTemplate [MustacheVariable True ["name"]])
        (object ["name" ~> ("<tag>" :: T.Text)])
      `shouldBe` return "&lt;tag&gt;"

    it "substitutes raw value for an unescaped variable" $
      substitute
        (toTemplate [MustacheVariable False ["name"]])
        (object ["name" ~> ("<tag>" :: T.Text)])
      `shouldBe` return "<tag>"

    it "substitutes a section when the key is present (and an empty object)" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" ~> object []])
      `shouldBe` return "t"

    it "substitutes a section when the key is present (and 'true')" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" ~> True])
      `shouldBe` return "t"

    it "substitutes a section once when the key is present and a singleton list" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" ~> ["True" :: T.Text]])
      `shouldBe` return "t"

    it "substitutes a section twice when the key is present and a list with two items" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" ~> (["True", "False"] :: [T.Text])])
      `shouldBe` return "tt"

    it "substitutes a section twice when the key is present and a list with two\
    \ objects, changing the scope to each object" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheVariable True ["t"]]])
        (object
          [ "section" ~>
            [ object ["t" ~> ("var1" :: T.Text)]
            , object ["t" ~> ("var2" :: T.Text)]
            ]
          ])
      `shouldBe` return "var1var2"

    it "does not substitute a section when the key is not present" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object [])
      `shouldBe` return ""

    it "does not substitute a section when the key is present (and 'false')" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" ~> False])
      `shouldBe` return ""

    it "does not substitute a section when the key is present (and empty list)" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" ~> ([] :: [T.Text])])
      `shouldBe` return ""

    it "substitutes a nested section" $
      substitute
        (toTemplate [MustacheVariable True ["outer", "inner"]])
        (object
          [ "outer" ~> object ["inner" ~> ("success" :: T.Text)]
          , "inner" ~> ("error" :: T.Text)
          ]
        )
        `shouldBe` return "success"


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
  let testfiles' = filter ((`elem` [".yml", ".yaml"]) . takeExtension) allFiles
      -- Filters the lambda tests for now.
      testfiles = filter (not . ("~" `isPrefixOf`) . takeFileName) testfiles'
  for_ testfiles $ \filename →
    runIO (decodeFile (dir </> filename)) >>= \case
      Nothing -> describe ("File: " <> takeFileName filename) $
        it "loads the data file" $
          expectationFailure "Data file could not be parsed"
      Just (LangSpecFile { tests }) →
        describe ("File: " <> takeFileName filename) $
          for_ tests $ \(LangSpecTest { .. }) →
            it ("Name: " <> name <> "  Description: " <> specDescription) $
              case parseTemplate name template of
                Left m → expectationFailure $ show m
                Right tmp →
                  case substituteValue tmp $ toMustache specData of
                    Left m → expectationFailure m
                    Right t → t `shouldBe` expected


main :: IO ()
main =
  void $
    withSystemTempDirectory
      "mustache-test-resources"
      $ \tempdir → do
        getOfficialSpecRelease tempdir
        hspec $ do
          parserSpec
          substituteSpec
          testOfficialLangSpec (tempdir </> langspecDir </> specDir)
