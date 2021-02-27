{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Main
  ( main
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Applicative ( (<$>), (<*>) )
import           Control.Exception ( Exception (..) )
import           Control.Lens ( (^.) )
import           Control.Monad ( mzero, void )
import           Data.ByteString.Lazy ( toStrict )
import           Data.Foldable ( for_ )
import qualified Data.HashMap.Strict as HM
import           Data.List ( isPrefixOf )
import           Data.Maybe ( fromMaybe, mapMaybe )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Yaml
                   ( FromJSON, (.!=), (.:), (.:?), decodeEither', parseJSON )
import qualified Data.Yaml as Y
import           Network.Wreq ( get, responseBody )
import           System.FilePath ( takeExtension, takeFileName )
import           Test.Hspec
                   ( Spec, describe, expectationFailure, hspec, shouldBe, it )
import           Text.Mustache
                   ( Template (..), ToMustache (..), (~>), catchSubstitute
                   , compileTemplate, object, substituteAST, substituteValue
                   )
import           Text.Mustache.Types
                   ( Context (..), Node (..), STree, SubM, Value (..) )


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
  , specData        :: Value
  , template        :: T.Text
  , expected        :: T.Text
  , testPartials    :: HM.HashMap String T.Text
  } deriving (Show)


instance FromJSON LangSpecFile where
  parseJSON (Y.Object o) = LangSpecFile
    <$> o .: "overview"
    <*> o .: "tests"
  parseJSON _ = mzero


instance FromJSON LangSpecTest where
  parseJSON (Y.Object o) = LangSpecTest
    <$> o .: "name"
    <*> o .: "desc"
    <*> fmap (toMustache @Y.Value) (o .: "data")
    <*> o .: "template"
    <*> o .: "expected"
    <*> o .:? "partials" .!= HM.empty
  parseJSON _ = mzero


getOfficialSpecRelease :: String -> IO [(String, LangSpecFile)]
getOfficialSpecRelease releaseURL  = do
    res <- get releaseURL
    let archive = Tar.read $ GZip.decompress (res ^. responseBody)
    either (error . show) (pure . fromEntries) $ entriesToList archive
  where
    entriesToList :: Tar.Entries e -> Either e [Tar.Entry]
    entriesToList Tar.Done = Right []
    entriesToList (Tar.Fail e) = Left e
    entriesToList (Tar.Next entry rest) = (entry:) <$> entriesToList rest

    fromEntries =
      map decodeSpec
      . filter (not . isOptionalSpec)
      . filter isYamlFile
      . mapMaybe fromNormalFile

    fromNormalFile e =
      case Tar.entryContent e of
        Tar.NormalFile f _ -> Just (Tar.entryPath e, f)
        _ -> Nothing

    isYamlFile (filename, _) = takeExtension filename `elem` [".yml", ".yaml"]

    isOptionalSpec (filename, _) = "~" `isPrefixOf` takeFileName filename

    decodeSpec (filename, f) =
      let spec = case decodeEither' $ toStrict f of
            Left e -> error $
                 "Error parsing spec file "
              ++ filename
              ++ ": "
              ++ displayException e
            Right spec -> spec
      in (filename, spec)


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
              pure $ template' { partials = partials' }
          in
            case compiled of
              Left m -> expectationFailure $ show m
              Right tmp ->
                substituteValue tmp specData `shouldBe` expected


-- | Defines the lambda functions that should be used to test the implementation.
--
-- If a test is not applicable (i.e. not possible) for an instance, skip the test
-- by setting Nothing.
--
-- If a test has all Nothing, then the current implementation is not fully
-- compliant with the spec, and steps should be taken to have at least one
-- instance able to satisfy the test.
data LambdaImplementations = LambdaImplementations
  { lambdaTreeToTreeM            :: Maybe (STree -> SubM STree)
  , lambdaContextAndTreeToTree   :: Maybe (Context Value -> STree -> STree)
  , lambdaContextAndTreeToText   :: Maybe (Context Value -> STree -> Text)
  , lambdaContextAndTreeToString :: Maybe (Context Value -> STree -> String)
  , lambdaTreeToTextM            :: Maybe (STree -> SubM Text)
  , lambdaTextToText             :: Maybe (Text -> Text)
  }

instance ToMustache LambdaImplementations where
  toMustache LambdaImplementations{..} = object
    [ "lambdaTreeToTreeM" ~> lambdaTreeToTreeM
    , "lambdaContextAndTreeToTree" ~> lambdaContextAndTreeToTree
    , "lambdaContextAndTreeToText" ~> lambdaContextAndTreeToText
    , "lambdaContextAndTreeToString" ~> lambdaContextAndTreeToString
    , "lambdaTreeToTextM" ~> lambdaTreeToTextM
    , "lambdaTextToText" ~> lambdaTextToText
    ]


-- | https://github.com/mustache/spec/blob/master/specs/~lambdas.yml
lambdaSpecs :: [(String, LangSpecFile)]
lambdaSpecs = flip map allLambdaImplementations $ \(key, label) ->
  ( "lambdas.yml (" ++ label ++ ")"
  , lambdaSpecFile { tests = mapMaybe (chooseLambda key) $ tests lambdaSpecFile }
  )
  where
    allLambdaImplementations =
      [ ("lambdaTreeToTreeM",            "STree -> SubM STree")
      , ("lambdaContextAndTreeToTree",   "Context Value -> STree -> STree")
      , ("lambdaContextAndTreeToText",   "Context Value -> STree -> Text")
      , ("lambdaContextAndTreeToString", "Context Value -> STree -> String")
      , ("lambdaTreeToTextM",            "STree -> SubM Text")
      , ("lambdaTextToText",             "Text -> Text")
      ]

    chooseLambda :: Text -> LangSpecTest -> Maybe LangSpecTest
    chooseLambda key test = fromMaybe (error $ "Could not set lambda in specData: " ++ show test) $ do
      Object o <- pure $ specData test
      Object lambdaImplementations <- HM.lookup "lambda" o
      HM.lookup key lambdaImplementations >>= \case
        v@(Lambda lambdaFunc) -> return $ Just $ test { specData = Object $ HM.insert "lambda" v o }
        Null -> return Nothing
        _ -> mzero

    lambdaSpecFile = LangSpecFile
      { overview = unlines
          [ "Lambdas are a special-cased data type for use in interpolations and"
          , "sections."
          , ""
          , "When used as the data value for an Interpolation tag, the lambda MUST be"
          , "treatable as an arity 0 function, and invoked as such.  The returned value"
          , "MUST be rendered against the default delimiters, then interpolated in place"
          , "of the lambda."
          , ""
          , "When used as the data value for a Section tag, the lambda MUST be treatable"
          , "as an arity 1 function, and invoked as such (passing a String containing the"
          , "unprocessed section contents).  The returned value MUST be rendered against"
          , "the current delimiters, then interpolated in place of the section."
          ]
      , tests =
        [ LangSpecTest
            { name = "Interpolation"
            , specDescription = "A lambda's return value should be interpolated."
            , specData = object
                -- "lambda": (_) => "world"
                [ "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Just $ \_ -> return [TextBlock "world"]
                    , lambdaContextAndTreeToTree = Just $ \_ _ -> [TextBlock "world"]
                    , lambdaContextAndTreeToText = Just $ \_ _ -> "world"
                    , lambdaContextAndTreeToString = Just $ \_ _ -> "world"
                    , lambdaTreeToTextM = Just $ \_ -> return "world"
                    , lambdaTextToText = Just $ \_ -> "world"
                    }
                ]
            , template = "Hello, {{lambda}}!"
            , expected = "Hello, world!"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Interpolation - Expansion"
            , specDescription = "A lambda's return value should be parsed."
            , specData = object
                -- "lambda": (_) => "{{planet}}"
                [ "planet" ~> T.pack "world"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Nothing
                    , lambdaContextAndTreeToTree = Nothing
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Nothing
                    , lambdaTextToText = Nothing
                    }
                ]
            , template = "Hello, {{lambda}}!"
            , expected = "Hello, world!"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Interpolation - Alternate Delimiters"
            , specDescription = "A lambda's return value should parse with the default delimiters."
            , specData = object
                -- "lambda": (_) => "|planet| => {{planet}}"
                [ "planet" ~> T.pack "world"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Nothing
                    , lambdaContextAndTreeToTree = Nothing
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Nothing
                    , lambdaTextToText = Nothing
                    }
                ]
            , template = "{{= | | =}}\nHello, (|&lambda|)!"
            , expected = "Hello, (|planet| => world)!"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Interpolation - Multiple Calls"
            , specDescription = "Interpolated lambdas should not be cached."
            , specData = object
                -- "lambda": (_) => { mutateGlobalState(); return someValue }
                [ "planet" ~> T.pack "world"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Nothing
                    , lambdaContextAndTreeToTree = Nothing
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Nothing
                    , lambdaTextToText = Nothing
                    }
                ]
            , template = "{{lambda}} == {{{lambda}}} == {{lambda}}"
            , expected = "1 == 2 == 3"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Escaping"
            , specDescription = "Lambda results should be appropriately escaped."
            , specData = object
                -- "lambda": (_) => ">"
                [ "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Just $ \_ -> return [TextBlock ">"]
                    , lambdaContextAndTreeToTree = Just $ \_ _ -> [TextBlock ">"]
                    , lambdaContextAndTreeToText = Just $ \_ _ -> ">"
                    , lambdaContextAndTreeToString = Just $ \_ _ -> ">"
                    , lambdaTreeToTextM = Just $ \_ -> return ">"
                    , lambdaTextToText = Just $ \_ -> ">"
                    }
                ]
            , template = "<{{lambda}}{{{lambda}}}"
            , expected = "<&gt;>"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Section"
            , specDescription = "Lambdas used for sections should receive the raw section string."
            , specData = object
                -- "lambda": (t) => if t == "{{x}}" then "yes" else "no"
                [ "x" ~> T.pack "Error!"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Nothing
                    , lambdaContextAndTreeToTree = Nothing
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Nothing
                    , lambdaTextToText = Nothing
                    }
                ]
            , template = "<{{#lambda}}{{x}}{{/lambda}}>"
            , expected = "<yes>"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Section - Expansion"
            , specDescription = "Lambdas used for sections should have their results parsed."
            , specData = object
                -- "lambda": (t) => t + "{{planet}}" + t
                [ "planet" ~> T.pack "Earth"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Nothing
                    , lambdaContextAndTreeToTree = Nothing
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Nothing
                    , lambdaTextToText = Nothing
                    }
                ]
            , template = "<{{#lambda}}-{{/lambda}}>"
            , expected = "<-Earth->"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Section - Alternate Delimiters"
            , specDescription = "Lambdas used for sections should parse with the current delimiters."
            , specData = object
                -- "lambda": (t) => t + "{{planet}} => |planet|" + t
                [ "planet" ~> T.pack "Earth"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Nothing
                    , lambdaContextAndTreeToTree = Nothing
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Nothing
                    , lambdaTextToText = Nothing
                    }
                ]
            , template = "{{= | | =}}<|#lambda|-|/lambda|>"
            , expected = "<-{{planet}} => Earth->"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Section - Multiple Calls"
            , specDescription = "Lambdas used for sections should not be cached."
            , specData = object
                -- "lambda": (t) => "__" + t + "__"
                [ "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Just $ \tree -> return $ [TextBlock "__"] <> tree <> [TextBlock "__"]
                    , lambdaContextAndTreeToTree = Just $ \_ tree -> [TextBlock "__"] <> tree <> [TextBlock "__"]
                    , lambdaContextAndTreeToText = Nothing
                    , lambdaContextAndTreeToString = Nothing
                    , lambdaTreeToTextM = Just $ \tree -> do
                        (_, res) <- catchSubstitute $ substituteAST tree
                        return $ "__" <> res <> "__"
                    , lambdaTextToText = Just $ \t -> "__" <> t <> "__"
                    }
                ]
            , template = "{{#lambda}}FILE{{/lambda}} != {{#lambda}}LINE{{/lambda}}"
            , expected = "__FILE__ != __LINE__"
            , testPartials = HM.empty
            }
        , LangSpecTest
            { name = "Inverted Section"
            , specDescription = "Lambdas used for inverted sections should be considered truthy."
            , specData = object
                -- "lambda": (_) => false
                [ "static" ~> T.pack "static"
                , "lambda" ~> LambdaImplementations
                    { lambdaTreeToTreeM = Just $ \_ -> return mempty
                    , lambdaContextAndTreeToTree = Just $ \_ _ -> mempty
                    , lambdaContextAndTreeToText = Just $ \_ _ -> mempty
                    , lambdaContextAndTreeToString = Just $ \_ _ -> mempty
                    , lambdaTreeToTextM = Just $ \_ -> return mempty
                    , lambdaTextToText = Just $ \_ -> mempty
                    }
                ]
            , template = "<{{^lambda}}{{static}}{{/lambda}}>"
            , expected = "<>"
            , testPartials = HM.empty
            }
        ]
      }


main :: IO ()
main =
  void $ do
    specs <- mapM getOfficialSpecRelease langspecs
    hspec $ mapM_ testOfficialLangSpec $ specs ++ [lambdaSpecs]
