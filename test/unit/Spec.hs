{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Data.Aeson
import           Data.Either
import qualified Data.Text            as T
import           Test.Hspec
import           Text.Mustache
import           Text.Mustache.AST
import           Text.Mustache.Parser


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


substituteSpec :: Spec
substituteSpec =
  describe "substitute" $ do

    let toTemplate ast' = MustacheTemplate "testsuite" ast' []

    it "substitutes a html escaped value for a variable" $
      substitute
        (toTemplate [MustacheVariable True ["name"]])
        (object ["name" .= ("<tag>" :: T.Text)])
      `shouldBe` return "&lt;tag&gt;"

    it "substitutes raw value for an unescaped variable" $
      substitute
        (toTemplate [MustacheVariable False ["name"]])
        (object ["name" .= ("<tag>" :: T.Text)])
      `shouldBe` return "<tag>"

    it "substitutes a section when the key is present (and an empty object)" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" .= object []])
      `shouldBe` return "t"

    it "substitutes a section when the key is present (and 'true')" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" .= True])
      `shouldBe` return "t"

    it "substitutes a section once when the key is present and a singleton list" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" .= ["True" :: T.Text]])
      `shouldBe` return "t"

    it "substitutes a section twice when the key is present and a list with two items" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" .= (["True", "False"] :: [T.Text])])
      `shouldBe` return "tt"

    it "substitutes a section twice when the key is present and a list with two\
    \ objects, changing the scope to each object" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheVariable True ["t"]]])
        (object
          [ "section" .=
            [ object ["t" .= ("var1" :: T.Text)]
            , object ["t" .= ("var2" :: T.Text)]
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
        (object ["section" .= False])
      `shouldBe` return ""

    it "does not substitute a section when the key is present (and empty list)" $
      substitute
        (toTemplate [MustacheSection ["section"] [MustacheText "t"]])
        (object ["section" .= ([] :: [T.Text])])
      `shouldBe` return ""


main :: IO ()
main = hspec $ do
  parserSpec
  substituteSpec
