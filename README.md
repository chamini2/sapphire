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

Compiling a Sapphire program

On the repository's root run the following commands:

1. cabal install
2. running the compiler:
    * `sapphire`: write a program directly on the command line, until `ctrl-D` is pressed.
    * `sapphire examples/tac/<file>.sp`: compiles a program written in the given file.
    * `sapphire --help`: displays a help menu.

Tips:   
Use cabal sandbox for better isolation of the execution environment.
