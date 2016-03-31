# cltk

This is a port of the [Ruby Language Toolkit](https://github.com/chriswailes/RLTK) to the [Crystal Programming Language](http://crystal-lang.org/).

*CLTK (like RLTK) is:* a collection of classes and methods designed to help programmers work with languages in an easy to use and straightforward manner.  This toolkit provides the following features:

* Lexer generator
* Parser generator
* AST node baseclass
* Class for representing context free grammars

In addition, CLTK includes several ready-made lexers and parsers. To see what works have a look at the specs or run them with:

```crystal
$ crystal spec
```

A *WIP* Example of a language designed with it can be found under `kazoo` (the naming is not very good, as this has nothing to do with the example language implementation provided by RLTK under the same name). The code is very hackey at the moment and was translated merely by try and error with the testsuite (`RED` > `GREEN` but so far no refactoring). Works with version 0.14.2 of the crystal compiler. The AST Implementation is a complete rewrite. LLVM Bindings are missing.

## Usage

See the example language under `kazoo` and its tests under `kazoo/specs`

## Contributing

1. Fork it ( https://github.com/ziprandom/cltk/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [ziprandom](https://github.com/ziprandom)  - creator, maintainer
