# cltk

This is a port of the [Ruby Language Toolkit](https://github.com/chriswailes/RLTK) to the [Crystal Programming Language](http://crystal-lang.org/).

**CLTK (like RLTK) is:** a collection of classes and methods designed to help programmers work with languages in an easy to use and straightforward manner.  This toolkit provides the following features:

* Lexer generator
* Parser generator
* AST node baseclass
* Class for representing context free grammars

In addition, CLTK includes several ready-made lexers and parsers. To see what works have a look at the specs or run them with:

```crystal
$ crystal spec
```

The code of the Parser is very hackey at the moment and was translated merely by try and error with the testsuite (`RED` > `GREEN` but so far no refactoring). The AST Implementation is a complete rewrite. The LLVM Bindings (`cltk/cg`) has not been ported. Instead crystals own LLVM Bindings were used in [`examples/kazoo/chapter 8/kcontractor.cr`](https://github.com/ziprandom/cltk/blob/master/examples/kazoo/chapter_8/kcontractor.cr)

## Usage

See the example languages (and their specs):
* **WIP** interpreted language EXP_LANG (`examples/exp_lang`)
* **WIP** **kazoo**  (`examples/kazoo/chapter 8`) with LLVM IR generation
* **brainfuck** (`examples/brainfuck`)

## Contributing

1. Fork it ( https://github.com/ziprandom/cltk/fork )
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

- [ziprandom](https://github.com/ziprandom)  - creator of the port, maintainer
