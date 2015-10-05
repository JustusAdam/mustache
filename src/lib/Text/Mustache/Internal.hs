{-|
Module      : $Header$
Description : Types and conversions
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}
module Text.Mustache.Internal (uncons) where


uncons ∷ [a] → Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = return (x, xs)
