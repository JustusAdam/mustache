
import Text.Mustache
import Data.Aeson
import qualified Data.ByteString.Lazy as BS


main = do
  f <- BS.readFile "test/data/simple.json"
  template <- compileTemplate "test/template/simple.mustache"
  print template
  case template of
    Left error -> print error
    Right template ->
      print $
        (eitherDecode f :: Either String Value) >>=
          substitute template
