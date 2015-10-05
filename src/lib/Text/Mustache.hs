{-|
Module      : $Header$
Description : Basic functions for dealing with mustache templates.
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

= How to use this library

This module exposes some of the most convenient functions for dealing with mustache
templates.

== Compiling with automatic partial discovery

The easiest way of compiling a file and its potential includes (called partials)
is by using the 'automaticCompile' function.

@
main :: IO ()
main = do
  let searchSpace = [".", "./templates"]
      templateName = "main.mustache"

  compiled <- automaticCompile searchSpace templateName
  case compiled of
    Left err -> print err
    Right template -> return () -- this is where you can start using it
@

The @searchSpace@ encompasses all directories in which the compiler should
search for the template source files.
tThe search itself is conducted in order, from left to right.

Should your search space be only the current working directory, you can use
'localAutomaticCompile'.

The @templateName@ is the relative path of the template to any directory
of the search space.

The compiler will throw errors if either the template is malformed
or the source file for a partial or the template itself could not be found
in any of the directories in @searchSpace@.

'automaticCompile' not only finds and compiles the template for you, it also
automatically recursively finds any partials included in the template as well,
compiles then and stores them in the 'partials' hash attached to the resulting
template.

== Substituting

In order to substitute data into the template it must be an instance of the 'ToMustache'
typeclass or be of type 'Value'.

This libray tries to imitate the API of <https://hackage.haskell.org/package/aeson aeson>
by allowing you to define conversions using a typeclass and operators.

=== Example

@
  data Person = { age :: Int, name :: String }

  instance ToMustache Person where
    toMustache person = object
      [ "age" ~> age person
      , "name" ~> name person
      ]
@

Values to the left of the '~>' operator have to be of type 'Text', hence the
@OverloadedStrings@ can become very hand here. Alternatively the '~~>' operator
can be used, which acepts arbitrary string-likes, converting them to text.

Values to the right of the '~>' operator must be an instance of the 'ToMustache'
typeclass. Alternatively, if your value to the right of the '~>' operator is
not an instance of 'ToMustache' but an instance of 'ToJSON' you can use the
'~=' operator, which accepts 'ToJSON' values.

All operators are also provided in a unicode form, for those that, like me, enjoy
unicode operators.

== Manual compiling

You can compile templates manually without requiring the IO monad at all using
the 'compileTemplate' function. This is the same function internally used by
'automaticCompile' and does not check if required partial are present.

More functions for manual compilation can be found in the 'Text.Mustache.Compile'
module. Including helpers for finding lists of partials in templates.

Additionally the 'compileTemplateWithCache' function is exposed here which you
may use to automatically compile a template but avoid some of the compilation
overhead by providing already compiled partials as well.

== Fundamentals

This library builds on three important data structures/types.

['Value'] A data structure almost identical to Data.Aeson.Value extended with
lambda functions which represents the data the template is being filled with.

['ToMustache'] A typeclass for converting arbitrary types to 'Value', similar
to Data.Aeson.ToJSON but with support for lambdas.

['Template'] Contains the 'AST', the abstract syntax tree. Basically a list of Text
or a mustache tags. The 'name' of the template and its 'partials' cache.

=== Compiling

During the compilation step the template file is found then parsed in a single
pass ('compileTemplate'), resulting in a 'Template' with an empty 'partials' section.

Subsequenty the 'AST' of the template is scanned for included partials, any
present 'TemplateCache' is queried for the partial ('compileTemplateWithCache'),
if not found it will be searched for in the @searchSpache@, compiled and
inserted into the template's own cache.

Internally no partial is compiled twice, as long as the names stay consistent.

Once compiled templates may be used multiple times for substitution or as
partial for other templates.

Partials are not being embedded into the templates during compilation, but during
substitution.

=== Substituting



-}
{-# LANGUAGE LambdaCase #-}
module Text.Mustache
  (
  -- * Compiling

  -- ** Automatic
    automaticCompile, localAutomaticCompile

  -- ** Manually
  , compileTemplateWithCache, compileTemplate, Template(..)

  -- * Rendering

  -- ** Generic

  , substitute

  -- ** Specialized

  , substituteValue

  -- ** Data Conversion
  , ToMustache, toMustache, object, (~>), (~=), (~~>), (~~=)
  ) where



import           Text.Mustache.Compile
import           Text.Mustache.Render
import           Text.Mustache.Types
