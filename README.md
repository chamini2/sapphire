Sapphire
========

Programming Language developed by Matteo Ferrando and Germán León

The [language design](doc/diseno.md) is currently on development.

---

Tools:

* `ghc`:   version 7.6.3
* `alex`:  version 3.0.5
* `happy`: version 1.18.10

*just install `haskell-platform`*

Compiling a Sapphire program:

1. cabal install
2. running the compiler:
    * `sapphire`: write a program directly on the command line, until `ctrl-D` is pressed.
    * `sapphire examples/tac/<file>.sp`: compiles a program written in the given file.
