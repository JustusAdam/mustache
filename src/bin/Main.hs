{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE UnicodeSyntax      #-}
module Main (main) where


import           Control.Applicative             ((<|>))
import           Data.Aeson                      (Value, eitherDecode)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BS
import           Data.Char                       (toLower)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text.IO                    as TIO
import qualified Data.Yaml                       as Y
import           System.Console.CmdArgs.Implicit
import           System.FilePath
import           Text.Mustache


data Arguments = Arguments
  { template     ∷ FilePath
  , templateDirs ∷ [FilePath]
  , dataFile     ∷ FilePath
  , outputFile   ∷ Maybe FilePath
  , dataFormat   ∷ Maybe String
  } deriving (Show, Data, Typeable)


commandArgs ∷ Arguments
commandArgs = Arguments
  { template  = def
      &= argPos 0
      &= typ "TEMPLATE"
  , dataFile  = def
      &= argPos 1
      &= typ "DATAFILE"
  , outputFile = Nothing
      &= help "Name of the resulting file"
      &= typFile
  , templateDirs = ["."]
      &= help "The directories in which to search for the templates"
      &= typ "DIRECTORIES"
  , dataFormat = Nothing
      &= help "Format for input data"
  } &= summary "Simple mustache template subtitution"


readJSON ∷ FilePath → IO (Either String Value)
readJSON = fmap eitherDecode . BS.readFile


readYAML ∷ FilePath → IO (Either String Value)
readYAML = fmap Y.decodeEither . B.readFile


readers ∷ [(String, FilePath → IO (Either String Value))]
readers =
  [ ("yaml", readYAML)
  , ("yml" , readYAML)
  , ("json", readJSON)
  ]


getReader ∷ String → Maybe (FilePath → IO (Either String Value))
getReader = flip lookup readers . map toLower


main ∷ IO ()
main = do
  a@(Arguments { dataFormat, template, templateDirs, dataFile, outputFile })
    ← cmdArgs commandArgs

  print a
  eitherTemplate ← compileTemplate templateDirs template

  case eitherTemplate of
    Left err → print err
    Right compiledTemplate → do

      let decode = fromMaybe readJSON $
            (dataFormat >>= getReader)
            <|> getReader (drop 1 $ takeExtension dataFile)

      let write = maybe TIO.putStrLn TIO.writeFile outputFile
      decoded ← decode dataFile

      either
        putStrLn
        write
        $ decoded >>=
          substitute compiledTemplate . toMustache
