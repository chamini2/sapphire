Características del lenguaje
============================

## Obligatorias

* Imperativo.
* Tipos fuertes.
* void, bool, enteros, flotantes, caracteres y cadenas.
* Alcance estático de bloques anidados.
* Selector.
* Iteración acotada.
* Iteración indeterminada.
* Pasaje por valor y por referencia
  * Tipos primitivos por valor o referencia.
  * Tipos agregados por referencia.
* Recursión.
* Estructuras arbitariamente anidadas (`struct` de C)
* Uniones arbitrariamente anidadas (`union` de C)
* Arreglos unidimensionales con índices arbitrarios
* Read, write polimórficos para tipos primitivos.

## Opcionales

* Arreglos multidimensionales base cero
* `break`, `continue` y etiquetas para bloques
* Seleccion múltiple (`case`)
* Funciones con número variable de argumentos - funciones variádicas
* Tipos enumeración / subrango / integrado con iteraciones
* Anidamiento de funciones -- implica cadena estática.
* Pasaje de parámetros "raros".
* Uniones con discriminante oculto.
* Funciones de segunda clase.

******

Programas a mostrar
===================

Para el día de la entrega final, todos deben proveer los siguientes
programas en sus lenguajes:

* Factorial iterativo.
* Factorial recursivo.
* Función de Ackermann.
* Solución a las Torres de Hanoi.
* Cálculo de raíz cuadrada usando el método de Newton.
* Producto punto y producto cruz de dos vectores. Los vectores tendrán no más de 20 dimensiones.
* Quicksort de un arreglo de 128 posiciones -- recursivo, en sitio.
* Heapsort de un arreglo de 128 posiciones -- recursivo, en sitio.
* Determinar si un arreglo de caracteres es "sub-arreglo" de otro
  arreglo de caracteres. Los dos arreglos de no más de 256 caracteres.

Para los que tienen arreglos de múltiples dimensiones:

* Multiplicación de matrices máximo de 10x10.
* Dijkstra sobre un grafo modelado con matriz de adyacencia.

Para los que tienen funciones de orden superior:

* Una función 'sort' que reciba un arreglo a ordenar, su tamaño y la
  función de comparación. Mostrar que ordena números enteros y
  caracteres.
* foldr sobre un arreglo. map sobre un arreglo.

BONUS POINTS

* Una calculadora postfija usando un reconocedor recursivo descendente.

Probablemente van a querer proveer programas adicionales para mostrar
que funciona el pasaje por referencia.
