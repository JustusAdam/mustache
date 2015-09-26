{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

* How to use this library

This module exposes some of the most convenient functions for dealing with mustache
templates.

The easiest way of compiling a file and its potential includes (called partials)
is by using the 'compileTemplate' function.
@
      -- the search space encompasses all directories in which the compiler should
      -- search for the template source files
      --
      -- the search is conducted in order, from left to right.
  let searchSpace = [".", "./templates"]
      -- the templateName is the relative path of the template to any directory
      -- of the search space
      templateName = "main.mustache"

    compiled <- automaticCompile searchSpace templateName
    case compiled of
      -- the compiler will throw errors if either the template is malformed
      -- or the source file for a partial could not be found
      Left err -> print err
      Right template -> return () -- this is where you can start using it
@

Should your search space be only the current working directory, you can use
'localAutomaticCompile'.

-}
{-# LANGUAGE LambdaCase #-}
module Text.Mustache
  (
  -- * Compiling

  -- ** Automatic
    automaticCompile, localAutomaticCompile

  -- ** Manually
  , compileTemplateWithCache, parseTemplate, Template(..)

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
