module Spec where


import Test.Hspec
import Text.Mustache.Parser
import Text.Mustache.AST
import Data.Monoid


parserSpec =
  describe "mustacheParseNode" $ do
    let parse = mustacheParser "testsuite"
    let returnedOne = Right . (:[])

    let text = "test12356p0--=-34{}jnv,\n"

    it "parses text" $
      parse text `shouldBe` returnedOne (MustacheText text)

    it "parses a variable" $
      parse "{{name}}" `shouldBe` returnedOne (MustacheVariable True "name")

    it "parses a variable with whitespace" $
      parse "{{ name  }}" `shouldBe` returnedOne (MustacheVariable True "name")

    it "allows '-' in variable names" $
      parse "{{ name-name }}" `shouldBe` returnedOne (MustacheVariable True "name-name")

    it "allows '_' in variable names" $
      parse "{{ name_name }}" `shouldBe` returnedOne (MustacheVariable True "name_name")

    it "parses a variable unescaped with {{{}}}" $
      parse "{{{name}}}" `shouldBe` returnedOne (MustacheVariable False "name")

    it "parses a variable unescaped with {{{}}} with whitespace" $
      parse "{{{  name     }}}" `shouldBe` returnedOne (MustacheVariable False "name")

    it "parses a variable unescaped with &" $
      parse "{{&name}}" `shouldBe` returnedOne (MustacheVariable False "name")

    it "parses a variable unescaped with & with whitespace" $
      parse "{{&  name  }}" `shouldBe` returnedOne (MustacheVariable False "name")

    it "parses a partial" $
      parse "{{>myPartial}}" `shouldBe` returnedOne (MustachePartial "myPartial")

    it "parses a partial with whitespace" $
      parse "{{>  myPartial }}" `shouldBe` returnedOne (MustachePartial "myPartial")

    it "parses the an empty section" $
      parse "{{#section}}{{/section}}" `shouldBe` returnedOne (MustacheSection "section" mempty)

    it "parses the an empty section with whitespace" $
      parse "{{#   section }}{{/     section }}" `shouldBe` returnedOne (MustacheSection "section" mempty)

main = hspec parserSpec
