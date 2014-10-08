##Intermediate Code

* Check if variable is in top scope or not -- We decided there's no variables in top scope, check anyway

* In any expression we should be using only temporals

* Check if `unapram` is done correctly

* Arrays
* Structures (Records/Unions)
* Initialize Variables

##General

* Functions have a `width` that **may be wrong**
* Decorate the AST `Either Expression (Expression, DataType)` -- or something like this
* Find strings in intermediate code (add it to the symbol table with a unique id or add it to symbol table by its content?) -- DONE, maybe
* Functions may not return some times
* Fix offsets for scopes

* data SymbolTable = SymTab (Map Identifier (Map ScopeNum Symbol))

##Someday

* Pointers

##Machine Code

* Graph library: fgl
