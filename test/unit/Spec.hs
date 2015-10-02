{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
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


parserSpec :: Spec
parserSpec =
  describe "mustacheParser" $ do
    let lparse = parse "testsuite"
    let returnedOne = return . return

    let text = "test12356p0--=-34{}jnv,\n"

    it "parses text" $
      lparse text `shouldBe` returnedOne (TextBlock text)

    it "parses a variable" $
      lparse "{{name}}" `shouldBe` returnedOne (Variable True (NamedData ["name"]))

    it "parses a variable with whitespace" $
      lparse "{{ name  }}" `shouldBe` returnedOne (Variable True (NamedData ["name"]))

    it "allows '-' in variable names" $
      lparse "{{ name-name }}" `shouldBe`
        returnedOne (Variable True (NamedData ["name-name"]))

    it "allows '_' in variable names" $
      lparse "{{ name_name }}" `shouldBe`
        returnedOne (Variable True (NamedData ["name_name"]))

    it "parses a variable unescaped with {{{}}}" $
      lparse "{{{name}}}" `shouldBe` returnedOne (Variable False (NamedData ["name"]))

    it "parses a variable unescaped with {{{}}} with whitespace" $
      lparse "{{{  name     }}}" `shouldBe`
        returnedOne (Variable False (NamedData ["name"]))

    it "parses a variable unescaped with &" $
      lparse "{{&name}}" `shouldBe` returnedOne (Variable False (NamedData ["name"]))

    it "parses a variable unescaped with & with whitespace" $
      lparse "{{&  name  }}" `shouldBe`
        returnedOne (Variable False (NamedData ["name"]))

    it "parses a partial" $
      lparse "{{>myPartial}}" `shouldBe`
        returnedOne (Partial (Just "") "myPartial")

    it "parses a partial with whitespace" $
      lparse "{{>  myPartial }}" `shouldBe`
        returnedOne (Partial (Just "") "myPartial")

    it "parses the an empty section" $
      lparse "{{#section}}{{/section}}" `shouldBe`
        returnedOne (Section (NamedData ["section"]) mempty)

    it "parses the an empty section with whitespace" $
      lparse "{{#   section }}{{/     section }}" `shouldBe`
        returnedOne (Section (NamedData ["section"]) mempty)

    it "parses a delimiter change" $
      lparse "{{=<< >>=}}<<var>>{{var}}" `shouldBe`
        return [Variable True (NamedData ["var"]), TextBlock "{{var}}"]

    it "parses a delimiter change with whitespace" $
      lparse "{{=<<   >>=}}<< var   >>{{var}}" `shouldBe`
        return [Variable True (NamedData ["var"]), TextBlock "{{var}}"]

    it "parses two subsequent delimiter changes" $
      lparse "{{=((  ))=}}(( var ))((=--  $-=))--#section$---/section$-" `shouldBe`
        return [Variable True (NamedData ["var"]), Section (NamedData ["section"]) []]

    it "propagates a delimiter change from a nested scope" $
      lparse "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldBe`
        return [Section (NamedData ["section"]) [], Variable True (NamedData ["var"])]

    it "fails if the tag contains illegal characters" $
      lparse "{{#&}}" `shouldSatisfy` isLeft

    it "parses a nested variable" $
      lparse "{{ name.val }}" `shouldBe` returnedOne (Variable True (NamedData ["name", "val"]))

    it "parses a variable containing whitespace" $
      lparse "{{ val space }}" `shouldBe` returnedOne (Variable True (NamedData ["val space"]))


substituteSpec :: Spec
substituteSpec =
  describe "substitute" $ do

    let toTemplate ast' = Template "testsuite" ast' mempty

    it "substitutes a html escaped value for a variable" $
      substitute
        (toTemplate [Variable True (NamedData ["name"])])
        (object ["name" ~> ("<tag>" :: T.Text)])
      `shouldBe` "&lt;tag&gt;"

    it "substitutes raw value for an unescaped variable" $
      substitute
        (toTemplate [Variable False (NamedData ["name"])])
        (object ["name" ~> ("<tag>" :: T.Text)])
      `shouldBe` "<tag>"

    it "substitutes a section when the key is present (and an empty object)" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> object []])
      `shouldBe` "t"

    it "substitutes a section when the key is present (and 'true')" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> True])
      `shouldBe` "t"

    it "substitutes a section once when the key is present and a singleton list" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> ["True" :: T.Text]])
      `shouldBe` "t"

    it "substitutes a section twice when the key is present and a list with two items" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> (["True", "False"] :: [T.Text])])
      `shouldBe` "tt"

    it "substitutes a section twice when the key is present and a list with two\
    \ objects, changing the scope to each object" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [Variable True (NamedData ["t"])]])
        (object
          [ "section" ~>
            [ object ["t" ~> ("var1" :: T.Text)]
            , object ["t" ~> ("var2" :: T.Text)]
            ]
          ])
      `shouldBe` "var1var2"

    it "does not substitute a section when the key is not present" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object [])
      `shouldBe` ""

    it "does not substitute a section when the key is present (and 'false')" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> False])
      `shouldBe` ""

    it "does not substitute a section when the key is present (and empty list)" $
      substitute
        (toTemplate [Section (NamedData ["section"]) [TextBlock "t"]])
        (object ["section" ~> ([] :: [T.Text])])
      `shouldBe` ""

    it "substitutes a nested section" $
      substitute
        (toTemplate [Variable True (NamedData ["outer", "inner"])])
        (object
          [ "outer" ~> object ["inner" ~> ("success" :: T.Text)]
          , "inner" ~> ("error" :: T.Text)
          ]
        )
        `shouldBe` "success"


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
                  partials' <- HM.traverseWithKey parseTemplate testPartials
                  template' <- parseTemplate name template
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
        hspec $ do
          parserSpec
          substituteSpec
          testOfficialLangSpec (tempdir </> langspecDir </> specDir)
