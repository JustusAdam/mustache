# mustache [![Travis Status](https://travis-ci.org/JustusAdam/mustache.svg?branch=master)](https://travis-ci.org/JustusAdam/mustache) [![Hackage](https://img.shields.io/hackage/v/mustache.svg)](https://hackage.haskell.org/package/mustache) [![Join the chat at https://gitter.im/JustusAdam/mustache](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/JustusAdam/mustache?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Haskell implementation of [mustache templates][mustache-homepage].

[mustache-homepage]: https://mustache.github.io

Implements the official [specs version 1.1.3](https://github.com/mustache/spec/releases/tag/v1.1.3)

## Motivation

The old Haskell implementation of mustache templates [hastache][] seemed pretty abandoned to me. This implementation aims to be much easier to use and (fingers crossed) better maintained.

[hastache]: https://hackage.haskell.org/package/hastache

Since it is so easy to use and requires but a few files of code, I've also written a small executable that compiles and renders mustache templates with data input from json or yaml files.

## Usage

### Library

Please refer to the [documentation][] on hackage.

[documentation]: https://hackage.haskell.org/package/mustache

### Executable `haskell-mustache`

    $ haskell-mustache --help
    Simple mustache template substitution

    arguments [OPTIONS] TEMPLATE [DATA-FILES]

    Common flags:
      -t --templatedirs[=DIRECTORY]  The directory in which to search for the
                                     templates
      -? --help                      Display help message
      -V --version                   Print version information

Current implementation substitutes the `TEMPLATE` once with each `DATA-FILE`

## Roadmap

- [x] String parser for mustache templates
- [x] Template substitution
- [x] Standalone executable
- [x] Support for 'set delimiter'
- [x] More efficiency using `Text` rather than `String`
- [x] More efficient Text parsing
- [x] Test coverage provided via the official [specs](https://github.com/mustache/spec)
- [x] Haddock documentation
- [ ] More instances for `ToMustache` typeclass
