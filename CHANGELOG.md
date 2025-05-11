# Mustache library changelog

## v2.4.3.1

- Renormalize line endings in test/unit/examples for `test-suite unit-tests`
  stanza to use LF convention.

## v2.4.3

- Adds compatibility with `text >= 2.1.2`.

## v2.4.2

- Also treat Null as falsey in inverted sections

## v2.4.1

- Compatibility with `containers >= 0.2.17`

## v2.4.0

- Support for aeson 2

## v2.3.2

- Added support for GHC 9.0.1

## v2.3.0

- Changed `EitherT` to `ExceptT` (deprecation)
- removed `getFile` from public API

## v2.2.3

- Quick fix to prevent catchSubstitute from reporting substitutions to the
  renderer.

## v2.2.2

- Added a function to catch a substitution result

## v2.2.1

- Quickfix for an issue with resolving in context

## v2.2

- changed substitution into a new monad
    + easier usage in lambdas and lambdas can now do nested substitution

## v2.1.4

- Treat Null as falsy in sections

## v2.1.3

- Added excaping for the apostrophe "'" as per xml spec, courtesy to @tfausak

## v2.1.2

- Fixed template cache again, as the spec requires access to the previous cache
  in partials as well

## v2.1.1

- Fixed an error where the substitution of partials would not use the template
  cache of the new partial

## v2.1

- Added API preserving checked substitution with 'checkedSubstitute' and
  'checkedSubstituteValue'
- Better and more ToMustache instances. No longer are all sequences of
  characters serialised as strings

## v2.0

- Added QuasiQuotes and template Haskell functions for compile time template
  embedding.

## v1.0

- Stabilised API's

## v0.5.1.0rc-7

- Removed dependency tagsoup
- Added ToMustache instances for some numbers

## v0.5.0.0rc-6

- Removed any dependency on ghc 7.10-type OverlappingInstances
- Resolved String/List overlapping instances

## v0.4.0.1rc-5

- Added a necessary OVERLAPPABLE pragma

## v0.4.0.0rc-4 (current stable version)

- Removed `conversion` and `conversion-text` dependency.
- Subsequently removed any dependency on overlapping instances
- Readded support for ghc version 7.8
- Removed `Char -> Value` instance of `ToMustache` (because of overlap)
- Renamed `AST`

## v0.3.1.0rc-3

- Added infix precedence to conversion operators
- Added `INLINEABLE` pragma to conversion functions

## v0.3.0.1rc-2

- Dropped GHC 7.8 support in favor of efficient and easy data conversion.

## v0.3.0.0rc-1

- improved documentation
- fixed a bug with scope
- small interface changes
