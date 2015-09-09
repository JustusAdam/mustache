{-# LANGUAGE CPP #-}
module Text.Mustache.Internal where


#if MIN_VERSION_base(4,8,0)
#else
  uncons :: [a] -> Maybe (a, [a])
  uncons [] = Nothing
  uncons (x:xs) = return (x, xs)
#endif
