{-# LANGUAGE OverloadedStrings #-}
module Main where


import Test.Hspec
import Text.Mustache.Parser
import Text.Mustache.AST
import Text.Mustache
import Data.Monoid
import Data.Aeson
import qualified Data.Text as T


parserSpec =
  describe "mustacheParser" $ do
    let parse = mustacheParser "testsuite"
    let returnedOne = return . return

    let text = "test12356p0--=-34{}jnv,\n"

    it "parses text" $
      parse text `shouldBe` returnedOne (MustacheText text)

    it "parses a variable" $
      parse "{{name}}" `shouldBe` returnedOne (MustacheVariable True "name")

    it "parses a variable with whitespace" $
      parse "{{ name  }}" `shouldBe` returnedOne (MustacheVariable True "name")

    it "allows '-' in variable names" $
      parse "{{ name-name }}" `shouldBe`
        returnedOne (MustacheVariable True "name-name")

    it "allows '_' in variable names" $
      parse "{{ name_name }}" `shouldBe`
        returnedOne (MustacheVariable True "name_name")

    it "parses a variable unescaped with {{{}}}" $
      parse "{{{name}}}" `shouldBe` returnedOne (MustacheVariable False "name")

    it "parses a variable unescaped with {{{}}} with whitespace" $
      parse "{{{  name     }}}" `shouldBe`
        returnedOne (MustacheVariable False "name")

    it "parses a variable unescaped with &" $
      parse "{{&name}}" `shouldBe` returnedOne (MustacheVariable False "name")

    it "parses a variable unescaped with & with whitespace" $
      parse "{{&  name  }}" `shouldBe`
        returnedOne (MustacheVariable False "name")

    it "parses a partial" $
      parse "{{>myPartial}}" `shouldBe`
        returnedOne (MustachePartial "myPartial")

    it "parses a partial with whitespace" $
      parse "{{>  myPartial }}" `shouldBe`
        returnedOne (MustachePartial "myPartial")

    it "parses the an empty section" $
      parse "{{#section}}{{/section}}" `shouldBe`
        returnedOne (MustacheSection "section" mempty)

    it "parses the an empty section with whitespace" $
      parse "{{#   section }}{{/     section }}" `shouldBe`
        returnedOne (MustacheSection "section" mempty)

    it "parses a delimiter change" $
      parse "{{=<< >>=}}<<var>>{{var}}" `shouldBe`
        return [MustacheVariable True "var", MustacheText "{{var}}"]

    it "parses a delimiter change with whitespace" $
      parse "{{=<<   >>=}}<< var   >>{{var}}" `shouldBe`
        return [MustacheVariable True "var", MustacheText "{{var}}"]

    it "parses two subsequent delimiter changes" $
      parse "{{=((  ))=}}(( var ))((=--  $-=))--#section$---/section$-" `shouldBe`
        return [MustacheVariable True "var", MustacheSection "section" []]

    it "propagates a delimiter change from a nested scope" $
      parse "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldBe`
        return [MustacheSection "section" [], MustacheVariable True "var"]


substituteSpec =
  describe "substitute" $ do

    let toTemplate ast = MustacheTemplate "testsuite" ast []

    it "substitutes a html escaped value for a variable" $
      substitute
        (toTemplate [MustacheVariable True "name"])
        (object ["name" .= ("<tag>" :: T.Text)])
      `shouldBe` return "&lt;tag&gt;"

    it "substitutes raw value for an unescaped variable" $
      substitute
        (toTemplate [MustacheVariable False "name"])
        (object ["name" .= ("<tag>" :: T.Text)])
      `shouldBe` return "<tag>"

    it "substitutes a section when the key is present (and an empty object)" $
      substitute
        (toTemplate [MustacheSection "section" [MustacheText "t"]])
        (object ["section" .= object []])
      `shouldBe` return "t"

main = hspec $ do
  parserSpec
  substituteSpec
