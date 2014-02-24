{
{-# OPTIONS -w #-}
module Parser( parseProgram, treePrint ) where

import Language
import Lexer
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexWrap } { TkEOF }
-- Without this we get a type error
%error { happyError }

%attributetype { Attribute a }
%attribute value        { a }
%attribute data_type    { Int }
%attribute len          { Int }

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
%left "or"
%left "and"
%right "not"

-- -- Compare
%nonassoc ">>"
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
    :                           { StNoop }      -- λ, no-operation
    | varid "=" Expression      { StAssign $1 $3 }

    -- Definitions
    | DataType VariableList     { StDeclaration $ map (\var -> Declaration var $1) $2 }
    --| FunctionDef               {  }
    | "return" Expression       { StReturn $2 }

    -- I/O
    | "read" VariableList       { StRead  (reverse $2) }
    | "print" ExpressionList    { StPrint (reverse $2) }

    -- Conditional
    | "if" ExpressionBool "then" StatementList "end"                          { StIf $2           (reverse $4) []           }
    | "if" ExpressionBool "then" StatementList "else" StatementList "end"     { StIf $2           (reverse $4) (reverse $6) }
    | "unless" ExpressionBool "then" StatementList "end"                      { StIf (NotBool $2) (reverse $4) []           }
    | "unless" ExpressionBool "then" StatementList "else" StatementList "end" { StIf (NotBool $2) (reverse $4) (reverse $6) }
    | "case" ExpressionArit CaseList "end"                                    { StCase $2 (reverse $3) []                   }
    | "case" ExpressionArit CaseList "else" StatementList "end"               { StCase $2 (reverse $3) (reverse $5)         }

    -- Loops
    | "while" ExpressionBool "do" StatementList "end"          { StWhile $2           (reverse $4) }
    | "until" ExpressionBool "do" StatementList "end"          { StWhile (NotBool $2) (reverse $4) }
    | "for" varid "in" ExpressionRang "do" StatementList "end" { StFor $2 $4 (reverse $6)          }
    | "break"           { StBreak }
    | "continue"        { StContinue }

Separator
    : ";"           {}
    | newline       {}

CaseList :: { [Case] }
    : Case              { [$1] }
    | CaseList Case     { $2 : $1 }

Case :: { Case }
    : "when" Expression "do" StatementList      { Case $2 (reverse $4) }

---------------------------------------

DataType :: { DataType }
    : "Int"         { Int }
    | "Float"       { Float }
    | "Bool"        { Bool }
    | "Char"        { Char }
    | "String"      { String }
    | "Range"       { Range }
    | "Type"        { Type }
    --| "Union" typeid
    --| "Record" typeid
--            ------------------------------ FALTA ARREGLOS

----DataTypeArray
----    : "[" DataType "]" "<-" "[" int "]"

VariableList :: { [Variable] }
    : varid                         { [$1]    }
    | VariableList "," varid        { $3 : $1 }

--FunctionDef :: { Function }
--    : "def" varid "::" Signature
--    | "def" varid "(" VariableList ")" "::" Signature "as" StatementList "end" -- length(ParemeterList) == length(Signature) - 1

--Signature :: { Signature }
--    : DataType
--    | Signature "->" DataType

---------------------------------------

Expression :: { Expression }
    : varid             { ExpressionId $1   }
    | ExpressionArit    { ExpressionArit $1 }
    | ExpressionBool    { ExpressionBool $1 }
    | ExpressionRang    { ExpressionRang $1 }
    | ExpressionStrn    { ExpressionStrn $1 }
    --| ExpressionArry    { ExpressionArry $1 }

ExpressionList :: { [Expression] }
    : Expression                        { [$1]    }
    | ExpressionList "," Expression     { $3 : $1 }

ExpressionArit :: { ExpressionArit }
    : int       { LiteralInt $1 }
    | float     { LiteralFloat $1 }
    | ExpressionArit "+" ExpressionArit     { PlusArit   $1 $3 }  -- { $1 + $3 }
    | ExpressionArit "-" ExpressionArit     { MinusArit  $1 $3 }  -- { $1 - $3 }
    | ExpressionArit "*" ExpressionArit     { TimesArit  $1 $3 }  -- { $1 * $3 }
    | ExpressionArit "/" ExpressionArit     { DivideArit $1 $3 }  -- { $1 / $3 }
    | ExpressionArit "%" ExpressionArit     { ModuloArit $1 $3 }  -- { $1 % $3 }
    | ExpressionArit "^" ExpressionArit     { PowerArit  $1 $3 }  -- { $1 ^ $3 }
    | "-" ExpressionArit                    { NegateArit $2    }  -- { negate $2 }

ExpressionBool :: { ExpressionBool }
    : "true"    { LiteralBool $1 }
    | "false"   { LiteralBool $1 }

    -- Booleans
    | ExpressionBool "or"  ExpressionBool   { OrBool  $1 $3 }  -- { $1 || $3 }
    | ExpressionBool "and" ExpressionBool   { AndBool $1 $3 }  -- { $1 && $3 }
    | "not" ExpressionBool                  { NotBool $2    }  -- { not $2   }

    -- Compare
    | ExpressionArit "==" ExpressionArit    { EqualBool   (ExpressionArit $1) (ExpressionArit $3) }  -- { $1 == $3 }
    | ExpressionArit "/=" ExpressionArit    { UnequalBool (ExpressionArit $1) (ExpressionArit $3) }  -- { $1 /= $3 }
    | ExpressionBool "==" ExpressionBool    { EqualBool   (ExpressionBool $1) (ExpressionBool $3) }  -- { $1 == $3 }
    | ExpressionBool "/=" ExpressionBool    { UnequalBool (ExpressionBool $1) (ExpressionBool $3) }  -- { $1 /= $3 }

    -- Arithmetic Compare
    | ExpressionArit "<"  ExpressionArit    { LessBool    $1 $3 }  -- { $1 <  $3 }
    | ExpressionArit "<=" ExpressionArit    { LessEqBool  $1 $3 }  -- { $1 <= $3 }
    | ExpressionArit ">"  ExpressionArit    { GreatBool   $1 $3 }  -- { $1 >  $3 }
    | ExpressionArit ">=" ExpressionArit    { GreatEqBool $1 $3 }  -- { $1 >= $3 }
    --| ExpressionArit ">>" ExpressionRang    { BelongsBool $1 $3 }  -- { $1 >> $3 }

ExpressionStrn :: { ExpressionStrn }
    : string    { LiteralStrn $1 }

ExpressionRang
    : ExpressionArit ".." ExpressionArit        { LiteralRang $1 $3 } -- $1.type = Int and $2.type= Int

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
