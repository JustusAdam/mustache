# mustache [![Travis Status](https://travis-ci.org/JustusAdam/mustache.svg?branch=master)](https://travis-ci.org/JustusAdam/mustache)

Haskell implementation of [mustache templates][mustache-homepage] based on instances of the ToJSON typeclass.

[mustache-homepage]: https://mustache.github.io

## Motivation

The old Haskell implementation of mustache tempates [hastache][] seemed pretty abandoned to me. This implementation aims to be much easier to use and (fingers crossed) better maintained.

[hastache]: https://hackage.haskell.org/package/hastache

Since it is so easy to use and requires but a few files of code, I've also written a small executable that compiles and renders mustache templates with data input from json or yaml files.

## Usage

### Library

... Soon™

### Executable `haskell-mustache`

    $ haskell-mustache --help
    Simple mustache template subtitution

    arguments [OPTIONS] TEMPLATE [DATA-FILES]

    Common flags:
      -t --templatedirs[=DIRECTORY]  The directory in which to search for the
                                     templates
      -? --help                      Display help message
      -V --version                   Print version information

Currenty substitutes the `TEMPLATE` once with each `DATA-FILE`

## Roadmap

- [x] String parser for mustache templates
- [x] Template substitution
- [x] Standalone executable
- [x] Support for 'set delimiter'
- [x] More efficiency using `Text` rather than `String`
- [ ] More efficient Text parsing using source positions
- [ ] Full unittest coverage
- [ ] Haddock documentation
