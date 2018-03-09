Safiro
======

Programming Language developed by Matteo Ferrando and Germán León

The [language design](doc/diseño.md) is currently on development, it's in spanish, though.

---

Tools:

* **`ghc`**:   version 7.8.3
* **`alex`**:  version 3.1.2
* **`happy`**: version 1.19.4

---

Compiling a Safiro program

On the repository's root run the following commands:

1. cabal install
2. using the compiler:
    * `safiro`: write a program directly on the command line, until `ctrl-D` is pressed.
    * `safiro examples/<file>.sp`: compiles a program written in the given file.
    * `safiro --help`: displays a help menu.

Tips:
* Use *cabal sandbox* for better isolation of the execution environment.
