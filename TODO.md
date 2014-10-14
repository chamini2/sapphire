##Intermediate Code

* Check if variable is in top scope or not -- We decided there's no variables in top scope, check anyway

* In any expression we should be using only temporals

* Arrays (not working for multiple dimensions)
* Structures (Records/Unions)
* Initialize Variables

##General

* check Printer.hs
* Functions have a `width` that **may be wrong**
* Decorate the AST `Either Expression (Expression, DataType)` -- or something like this
* Find strings in intermediate code (add it to the symbol table with a unique id or add it to symbol table by its content?) -- DONE (right?)
* Functions may not return some times -- DONE
* Fix offsets for scopes
* Create missing Error construtors (mostly in Parser.y, there are more) -- DONE

* data SymbolTable = SymTab (Map Identifier (Map ScopeNum Symbol))
* add applicative code for Lexeme
* change name `Lexeme`

* remove self-assign from design or add it to the language

* pass arrays by reference (width changes between parameter and variable)

##Ask

* What should be happening

    def a : Int[2][3] c -> ()
    end
    d : Int[2][3][4]
    e : Int[4][2][3]
    a(d[0])
    a(e[0])
    Static error at 6:1:
        FunctionArguments "a" (fromList [Int[2][3]]) (fromList [Int[4][2]])

In C++ the following happens:
    
    int main(int argc, char *argv[]) {
        int x[2][3][4] ;

        /* 24 */ cout << sizeof(x) / 4 << endl;
        /* 12 */ cout << sizeof(x[0]) / 4 << endl;
        /* 4  */ cout << sizeof(x[0][0]) / 4 << endl;
        /* 1  */ cout << sizeof(x[0][0][0]) / 4 << endl;
        return 0;
    }

Look at this

    data Expression = Access Identifier [Access]

    data Access = StructAccess Identifier
                | ArrayAccess  Expression

    data DataType = Array DataType Int

    -- a[0].x[1].c
    -- Access "a" [ArrayAccess 0, StructAccess "x", ArrayAccess 1, StructAccess "c"]

##Someday

* Pointers

##Machine Code

* Graph library: fgl
