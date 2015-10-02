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


#if MIN_VERSION_base(4,8,0)
import Data.List (uncons)
#else
uncons ∷ [a] → Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = return (x, xs)
#endif
