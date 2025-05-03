{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Main (main) where


import           Control.Exception               (Exception (..))
import           Data.Aeson                      (Value, eitherDecode)
import           Data.Bifunctor                  (first)
import qualified Data.ByteString                 as B (readFile)
import qualified Data.ByteString.Lazy            as BS (readFile)
import           Data.Foldable                   (for_)
import qualified Data.Text.IO                    as TIO (putStrLn)
import           Data.Yaml                       (decodeEither')

import           System.Console.CmdArgs.Implicit (Data, Typeable, argPos, args,
                                                  cmdArgs, def, help, summary,
                                                  typ, (&=))
import           System.Exit                     (exitFailure)
import           System.FilePath                 (takeExtension)
import           System.IO                       (hPutStrLn, stderr)
import           Text.Mustache                   (automaticCompile, substitute,
                                                  toMustache)


data Arguments = Arguments
  { template     :: FilePath
  , templateDirs :: [FilePath]
  , dataFiles    :: [FilePath]
  } deriving (Show, Data, Typeable)


commandArgs :: Arguments
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


readJSON :: FilePath -> IO (Either String Value)
readJSON = fmap eitherDecode . BS.readFile


readYAML :: FilePath -> IO (Either String Value)
readYAML = fmap (first displayException . decodeEither') . B.readFile


main :: IO ()
main = do
  (Arguments { template, templateDirs, dataFiles }) <- cmdArgs commandArgs

  eitherTemplate <- automaticCompile templateDirs template

  case eitherTemplate of
    Left err -> handleError $
#if MIN_VERSION_parsec(3,1,17)
      displayException err
#else
      show err
#endif
    Right compiledTemplate ->
      for_ dataFiles $ \file -> do

        let decoder =
              case takeExtension file of
                ".yml"  -> readYAML
                ".yaml" -> readYAML
                _       -> readJSON
        decoded <- decoder file

        either
          handleError
          (TIO.putStrLn . substitute compiledTemplate . toMustache)
          decoded
 where
  handleError msg = hPutStrLn stderr msg >> exitFailure
