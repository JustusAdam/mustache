{-|
Module      : $Header$
Description : Types and conversions
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

escapeXML and xmlEntities curtesy to the tagsoup library.
-}
module Text.Mustache.Internal (uncons, escapeXMLText) where


import           Data.Char       (ord)
import qualified Data.IntMap     as IntMap
import qualified Data.Text       as T
import           Prelude.Unicode


uncons :: [α] -> Maybe (α, [α])
uncons []     = Nothing
uncons (x:xs) = return (x, xs)


escapeXMLText :: T.Text -> T.Text
escapeXMLText = T.pack . escapeXML . T.unpack


escapeXML :: String -> String
escapeXML = concatMap $ \x -> IntMap.findWithDefault [x] (ord x) mp
    where mp = IntMap.fromList [(ord b, "&"++a++";") | (a,[b]) <- xmlEntities]


xmlEntities :: [(String, String)]
xmlEntities =
  [ ("quot", "\"")
  , ("amp" , "&")
  , ("lt"  , "<")
  , ("gt"  , ">")
  ]
