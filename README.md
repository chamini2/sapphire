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

1. cd to path *src/*
2. `make`
3. running the compiler:
    * `make run`: write a program directly on the command line, until `ctrl-D` is pressed.
    * `make run ../examples/<file>.sp`: compiles a program written in the given file.


Extra usage:

- `make clean`: erase all compilation files.
- `make happyinfo`: generates the file `info.txt` with the grammar information.
