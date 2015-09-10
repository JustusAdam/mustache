{-# LANGUAGE LambdaCase #-}
module Text.Mustache
  (
  -- * Compiling

  -- ** Automatic
    compileTemplate

  -- ** Manually
  , compileTemplateWithCache, parseTemplate

  -- * Rendering

  -- ** Generic

  , substitute

  -- ** Specialized

  , substituteValue

  -- * Data structures
  , MustacheTemplate(..), ToMustache, toMustache

  -- * Util

  -- | These are functions used internally by the parser and renderer. Whether
  -- these  will continue to be exposed is to be seen.
  , getFile , getPartials , getPartials', toString, search, Context(..)
  ) where



import           Text.Mustache.Compile
import           Text.Mustache.Render
import           Text.Mustache.Types
