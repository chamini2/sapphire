{
{-# OPTIONS -w #-}
module Parser( parseProgram ) where

import Language
import Lexer
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexWrap } { TkEOF }
-- Without this we get a type error
%error { happyError }

--%attributetype { Attribute a }
--%attribute value { a }
--%attribute num   { Int }
--%attribute label { String }

%token

        -- Language
        newline         { TkNewLine    }
        "main"          { TkMain       }
        "begin"         { TkBegin      }
        "end"           { TkEnd        }
        "return"        { TkReturn     }
        ";"             { TkSemicolon  }
        ","             { TkComma      }

        -- -- Brackets
        "("             { TkLParen     }
        ")"             { TkRParen     }
        "["             { TkLBrackets  }
        "]"             { TkRBrackets  }
        "{"             { TkLBraces    }
        "}"             { TkRBraces    }

        -- Types
        "Void"          { TkVoidType   }
        "Int"           { TkIntType    }
        "Bool"          { TkBoolType   }
        "Float"         { TkFloatType  }
        "Char"          { TkCharType   }
        "String"        { TkStringType }
        "Range"         { TkRangeType  }
        "Union"         { TkUnionType  }
        "Record"        { TkRecordType }
        "Type"          { TkTypeType   }

        -- Statements
        -- -- Declarations
        "="             { TkAssign     }
        "def"           { TkDef        }
        "as"            { TkAs         }
        "::"            { TkSignature  }
        "->"            { TkArrow      }

        -- -- In/Out
        "read"          { TkRead       }
        "print"         { TkPrint      }

        -- -- Conditionals
        "if"            { TkIf         }
        "then"          { TkThen       }
        "else"          { TkElse       }
        "unless"        { TkUnless     }
        "case"          { TkCase       }
        "when"          { TkWhen       }

        -- -- Loops
        "for"           { TkFor        }
        "in"            { TkIn         }
        ".."            { TkFromTo     }
        "do"            { TkDo         }
        "while"         { TkWhile      }
        "until"         { TkUntil      }
        "break"         { TkBreak      }
        "continue"      { TkContinue   }

        -- Expressions/Operators
        -- -- Literals
        int             { TkInt $$     }
        "true"          { TkTrue $$    }
        "false"         { TkFalse $$   }
        float           { TkFloat $$   }
        string          { TkString $$  }

        -- -- Num
        "+"             { TkPlus       }
        "-"             { TkMinus      }
        "*"             { TkTimes      }
        "/"             { TkDivide     }
        "%"             { TkModulo     }
        "^"             { TkPower      }

        -- -- Bool
        "or"            { TkOr         }
        "and"           { TkAnd        }
        "not"           { TkNot        }
        "=="            { TkEqual      }
        "/="            { TkUnequal    }
        "<"             { TkLess       }
        ">"             { TkGreat      }
        "<="            { TkLessEq     }
        ">="            { TkGreatEq    }

        -- -- Identifiers
        varid           { TkVarId $$   }
        typeid          { TkTypeId $$  }

-------------------------------------------------------------------------------
-- Precedence

-- Bool
%left  "or" "and"
%right "not"

-- -- Compare
%nonassoc ">>"        -- Que es esto?
%nonassoc "==" "/="
%nonassoc "<" "<=" ">" ">="

-- Arithmetic
%left "+" "-"
%left "*" "/" "%"
%left ".."
%right "-"
%right "^"

%%

-------------------------------------------------------------------------------
-- Grammar

Program :: { Program }
    : StatementList         { reverse $1 }

StatementList :: { [Statement] }
    : Statement                             { [$1]    }
    | StatementList Separator Statement     { $3 : $1 }

Statement :: { Statement }
    :                           { NoOp } -- λ
    | varid "=" Expression      { Assign $1 $3 }
--    | DataType VariableList
--    | FunctionDef
--    | "return" Expression
--    | "read" VariableList
--    | "print" ExpressionList
--    | "if" ExpressionBool "then" StatementList
--    | "if" ExpressionBool "then" StatementList "else" StatementList

Separator
    : ";"           {}
    | newline       {}

---------------------------------------

--DataType :: { DataType }
--    : "Int"
--    | "Float"
--    | "Bool"
--    | "Char"
--    | "String"
--    | "Range"
--    | "Type"
--    | "Union" typeid
--    | "Record" typeid
--            ------------------------------ FALTA ARREGLOS

----DataTypeArray
----    : "[" DataType "]" "<-" "[" int "]"

--VariableList :: { [VarName] }
--    : varid
--    | VariableList "," varid

--FunctionDef :: { Function }
--    : "def" varid "::" Signature
--    | "def" varid "(" VariableList ")" "::" Signature "as" StatementList "end" -- length(ParemeterList) == length(Signature) - 1

--Signature :: { Signature }
--    : DataType
--    | Signature "->" DataType

---------------------------------------
CompOp :: { CompOp }                          
        : '=='                               { Eq  }
        | '/='                               { Neq }
        | '<='                               { Leq }
        | '<'                                { Lt  }
        | '>='                               { Geq }
        | '>'                                { Gt  }

OpAdd :: { BinOp }
       : '+'                                { Add }
       | '-'                                { Sub }
       | 'or'                               { Or  }
       | 'and'                              { And }
       | '*'                                { Mul }
       | '^'                                { Pow }
       | '/'                                { Div }
       | '%'                                { Mod }
                                                
UnOp :: { UnOp }                          
        : '-'                              { AritNeg }
        | 'not'                            { BoolNeg }

B :: { Analyzer Expression }
  : B CompOp E                                       { EComp $2 $1 $3 }
  | E                                                { $1 }

E :: { Analyzer Expression }
  : E BinOp F                                        { EBin $2 $1 $3 }
  | F                                                { $1 }

F :: { Exp }
  : ident                                            { EVar $1 Null } 
  | num                                              { EConst $1 Null }
  | UnOp F                                           { EUnOp $1 $2 Null }
  | 'true'                                           { ETrue }
  | 'false'                                          { EFalse }
  | '(' B ')'                                        { EBrack $2 }     
------------------------------ VIEJO ------------------------------------------

Expression
    : ExpressionArit    { ExpressionArit $1 }
    | ExpressionBool    { ExpressionBool $1 }
    | ExpressionStrn    { ExpressionStrn $1}
--    | ExpressionRang
--    | ExpressionArry

--ExpressionList
--    : Expression
--    | ExpressionList "," Expression

ExpressionArit
    : int       { LiteralInt $1 }
    | float     { LiteralFloat $1 }
    | ExpressionArit "+" ExpressionArit     { PlusArit $1 $3 }  -- { $1 + $3 }
--    | ExpressionArit "-" ExpressionArit
--    | ExpressionArit "*" ExpressionArit
--    | ExpressionArit "/" ExpressionArit
--    | ExpressionArit "%" ExpressionArit
--    | ExpressionArit "^" ExpressionArit
--    | "-" ExpressionArit

ExpressionBool
    : "true"    { LiteralBool $1 }
    | "false"   { LiteralBool $1 }
    | ExpressionBool "or"  ExpressionBool   { OrBool $1 $3  }  -- { $1 || $3 }
    | ExpressionBool "and" ExpressionBool   { AndBool $1 $3 }  -- { $1 && $3 }
    | "not" ExpressionBool                  { NotBool $2    }  -- { not $2   }
--    | Expression      "=="  Expression
--    | Expression      "/="  Expression
--    | ExpressionArit "<"   ExpressionArit
--    | ExpressionArit "<="  ExpressionArit
--    | ExpressionArit ">"   ExpressionArit
--    | ExpressionArit ">="  ExpressionArit
--    | ExpressionArit ">>"  ExpressionRang

ExpressionStrn
    : string    { LiteralStrn $1 }

--ExpressionRang
--    : ExpressionArit ".." ExpressionArit -- $1.type = Int and $2.type= Int

------------------------------ VIEJO ------------------------------------------

--Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
--      | Exp1                    { Exp1 $1 }

--Exp1  : Exp1 '+' Term           { Plus $1 $3 }
--      | Exp1 '-' Term           { Minus $1 $3 }
--      | Term                    { Term $1 }

--Term  : Term '*' Factor         { Times $1 $3 }
--      | Term '/' Factor         { Div $1 $3 }
--      | Factor                  { Factor $1 }

--Factor
--      : int                     { Int $1 }
--      | var                     { Var $1 }
--      | '(' Exp ')'             { Brack $2 }

{

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap cont = do
  t <- alexMonadScan
  cont t

getPosn :: Alex (Int, Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

happyError :: Token -> Alex a
happyError t = do
  (l,c) <- getPosn
  fail (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")

parseProgram :: String -> Either String Program
parseProgram s = runAlex s parse

}
