# Mustache library changelog

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
