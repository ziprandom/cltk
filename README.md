# cltk [![Build Status](https://api.travis-ci.org/ziprandom/cltk.svg)](https://travis-ci.org/ziprandom/cltk)

This is a port of the [Ruby Language Toolkit](https://github.com/chriswailes/RLTK) to the [Crystal Programming Language](http://crystal-lang.org/).

**CLTK (like RLTK) is:** a collection of classes and methods designed to help programmers create languages in an easy to use and straightforward manner. This toolkit provides the following features:

* Lexer generator
* Parser generator
* AST node baseclass
* Class for representing context free grammars

In addition, CLTK includes several ready-made lexers and parsers. As well as a [serialization mechanism](src/cltk/parser/crystalize.cr) that renders a finalized parser back into crystal syntax to compile it without having to be finalized again at startup (see [exp_lang_repl](examples/exp_lang/exp_lang_repl.cr) for a usage example).

To see what works have a look at the specs or run them with:

```crystal
$ crystal spec
```

The Implementation has been strongly altered but the API is compatible to RLTK. The LLVM Bindings has not been ported. Instead crystals own LLVM Bindings were used in [`examples/kazoo/chapter 8/kcontractor.cr`](https://github.com/ziprandom/cltk/blob/master/examples/kazoo/chapter_8/kcontractor.cr)

## Usage

See the example languages (and their specs):
* **interpreted language EXP_LANG** (`examples/exp_lang`)
* **kazoo**  (`examples/kazoo/chapter 8`) with LLVM IR generation
* **brainfuck** (`examples/brainfuck`)
* **json_parser** (`examples/json_parser`) example of a linter / xml converter

## Contributing

1. Fork it ( https://github.com/ziprandom/cltk/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [ziprandom](https://github.com/ziprandom)  - creator of the port, maintainer
