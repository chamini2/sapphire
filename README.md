Sapphire
========

Lenguaje de programación desarrollado por Matteo Ferrando y Germán León.

El [diseño](doc/diseno.md) del lenguaje sigue en desarrollo.

---

Estamos usando las siguientes herramientas:

* `ghc`: version 7.6.3
* `alex`: version 3.0.5
* `happy`: version 1.18.10

*basta instalar `haskell-platform`*

Para compilar:

1. entrar en *src/*
2. `make`
3. correr un programa:
    * `make run`: esto te permite escribir un programa directamente, hasta presionar `ctrl-D`
    * `make run ../examples/<file>.sp`: esto te permite correr un programa directamente en Sapphire.

Extras:

- `make clean`: borra archivos de compilación.
- `make happyinfo`: crea un archivo `info.txt` con la información de la gramática
y el parser que genera Happy.
