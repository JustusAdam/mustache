{-|
Module      : $Header$
Description : Types and conversions
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}
module Text.Mustache.Internal (uncons, convertT, TextConvertible) where


import           Data.Text hiding (uncons)


uncons ∷ [a] → Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = return (x, xs)


class TextConvertible a where
  convertT ∷ a → Text

instance TextConvertible Text where
  convertT = id

instance TextConvertible String where
  convertT = pack
