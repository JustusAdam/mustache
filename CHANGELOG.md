# Mustache library changelog

## v2.1.1

- Fixed an error where the substitution of partials would not use the template cache of the new partial

## v2.1

- Added API preserving checked substitution with 'checkedSubstitute' and 'checkedSubstituteValue'
- Better and more ToMustache instances. No longer are all sequences of characters serialised as strings

## v2.0

- Added QuasiQuotes and template Haskell functions for compile time template embedding.

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

Dropped GHC 7.8 support in favor of efficient and easy data conversion.

## v0.3.0.0rc-1

- improved documentation
- fixed a bug with scope
- small interface changes
