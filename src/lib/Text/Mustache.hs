{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE LambdaCase #-}
module Text.Mustache
  (
  -- * Compiling

  -- ** Automatic
    compileTemplate

  -- ** Manually
  , compileTemplateWithCache, parseTemplate, MustacheTemplate(..)

  -- * Rendering

  -- ** Generic

  , substitute

  -- ** Specialized

  , substituteValue

  -- ** Data Conversion
  , ToMustache, toMustache, object, (~>), (~=), (~~>), (~~=)

  -- * Util

  -- | These are functions used internally by the parser and renderer. Whether
  -- these  will continue to be exposed is to be seen.
  , getFile , getPartials , getPartials', toString, search, Context(..)
  ) where



import           Text.Mustache.Compile
import           Text.Mustache.Render
import           Text.Mustache.Types
