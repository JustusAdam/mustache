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
