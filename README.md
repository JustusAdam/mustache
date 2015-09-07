# mustache

Haskell implementation of [mustache templates][mustache-homepage] based on instances of the ToJSON typeclass.

[mustache-homepage]: https://mustache.github.io

## Motivation

The old Haskell implementation of mustache tempates [hastache][] seemed pretty abandoned to me. This implementation aims to be much easier to use and (fingers crossed) better maintained.

[hastache]: https://hackage.haskell.org/package/hastache

Since it is so easy to use and requires but a few files of code, I've also written a small executable that compiles and renders mustache templates with data input from json or yaml files.

## Usage

... Soon™

## Roadmap

- [x] String parser for mustache templates
- [x] Template substitution
- [x] Standalone executable
- [ ] Support for 'set delimiter'
- [ ] More efficiency using `Text` rather than `String`
- [ ] More efficient Text parsing using source positions
