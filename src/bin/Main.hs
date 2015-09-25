{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE UnicodeSyntax      #-}
module Main (main) where


import           Data.Aeson
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BS
import           Data.Foldable
import qualified Data.Text.IO                    as TIO
import qualified Data.Yaml                       as Y
import           System.Console.CmdArgs.Implicit
import           System.FilePath
import           Text.Mustache


data Arguments = Arguments
  { template     :: FilePath
  , templateDirs :: [FilePath]
  , dataFiles    :: [FilePath]
  } deriving (Show, Data, Typeable)


commandArgs ∷ Arguments
commandArgs = Arguments
  { template = def
      &= argPos 0
      &= typ "TEMPLATE"
  , dataFiles = def
      &= args
      &= typ "DATA-FILES"
  , templateDirs = ["."]
      &= help "The directories in which to search for the templates"
      &= typ "DIRECTORIES"
  } &= summary "Simple mustache template subtitution"


readJSON ∷ FilePath → IO (Either String Value)
readJSON = fmap eitherDecode . BS.readFile


readYAML ∷ FilePath → IO (Either String Value)
readYAML = fmap Y.decodeEither . B.readFile


main ∷ IO ()
main = do
  a@(Arguments { template, templateDirs, dataFiles }) <- cmdArgs commandArgs

  print a
  eitherTemplate ← compileTemplate templateDirs template

  case eitherTemplate of
    Left err → print err
    Right compiledTemplate →
      for_ dataFiles $ \file → do

        let decoder =
              case takeExtension file of
                ".yml" → readYAML
                ".yaml" → readYAML
                _ → readJSON
        decoded ← decoder file

        either
          putStrLn
          TIO.putStrLn
          $ substitute compiledTemplate . toMustache <$> decoded
