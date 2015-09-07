{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where


import           Data.Aeson
import qualified Data.ByteString.Lazy            as BS
import           Data.Foldable
import           System.Console.CmdArgs.Implicit
import           Text.Mustache


data Arguments = Arguments
  { template     :: FilePath
  , templateDirs :: [FilePath]
  , dataFiles    :: [FilePath]
  } deriving (Show, Data, Typeable)


commandArgs = Arguments
  { template = def
      &= argPos 0
      &= typ "TEMPLATE"
  , dataFiles = def
      &= args
      &= typ "DATA-FILES"
  , templateDirs = def
      &= help "The directory in which to search for the templates"
      &= opt "."
      &= typ "DIRECTORY"
  } &= summary "Simple mustache template subtitution"


main = do
  (Arguments { template, templateDirs, dataFiles }) <- cmdArgs commandArgs

  ctemplate <- compileTemplate templateDirs template

  case ctemplate of
    Left error -> print error
    Right template ->
      for_ dataFiles $ \file -> do
        f <- BS.readFile file

        either
          putStrLn
          putStrLn
          $ (eitherDecode f :: Either String Value) >>=
            substitute template
