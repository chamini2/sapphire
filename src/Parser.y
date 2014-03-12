{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Checker

import           Prelude
import qualified Data.Foldable as DF
import           Data.Sequence
--import           Control.Monad.RWS
--import           Control.Monad
}

%name parse
--%tokentype { Lexeme }
%tokentype { Token }
%monad { Alex }
--%lexer { lexWrap } { Lex TkEOF _ }
%lexer { lexWrap } { TkEOF }
-- Without this we get a type error
%error { happyError }

%token

        -- Language
        --newline         { Lex TkNewLine     _ }
        --"main"          { Lex TkMain        _ }
        --"begin"         { Lex TkBegin       _ }
        --"end"           { Lex TkEnd         _ }
        --"return"        { Lex TkReturn      _ }
        --";"             { Lex TkSemicolon   _ }
        --","             { Lex TkComma       _ }

        -- -- Brackets
        --"("             { Lex TkLParen      _ }
        --")"             { Lex TkRParen      _ }
        --"["             { Lex TkLBrackets   _ }
        --"]"             { Lex TkRBrackets   _ }
        --"{"             { Lex TkLBraces     _ }
        --"}"             { Lex TkRBraces     _ }

        -- Types
        --"Void"          { Lex TkVoidType    _ }
        --"Int"           { Lex TkIntType     _ }
        --"Bool"          { Lex TkBoolType    _ }
        --"Float"         { Lex TkFloatType   _ }
        --"Char"          { Lex TkCharType    _ }
        --"String"        { Lex TkStringType  _ }
        --"Range"         { Lex TkRangeType   _ }
        --"Union"         { Lex TkUnionType   _ }
        --"Record"        { Lex TkRecordType  _ }
        --"Type"          { Lex TkTypeType    _ }

        -- Statements
        -- -- Declarations
        --"="             { Lex TkAssign      _ }
        --"def"           { Lex TkDef         _ }
        --"as"            { Lex TkAs          _ }
        --"::"            { Lex TkSignature   _ }
        --"->"            { Lex TkArrow       _ }

        -- -- In/Out
        --"read"          { Lex TkRead        _ }
        --"print"         { Lex TkPrint       _ }

        -- -- Conditionals
        --"if"            { Lex TkIf          _ }
        --"then"          { Lex TkThen        _ }
        --"else"          { Lex TkElse        _ }
        --"unless"        { Lex TkUnless      _ }
        --"case"          { Lex TkCase        _ }
        --"when"          { Lex TkWhen        _ }

        -- -- Loops
        --"for"           { Lex TkFor         _ }
        --"in"            { Lex TkIn          _ }
        --".."            { Lex TkFromTo      _ }
        --"do"            { Lex TkDo          _ }
        --"while"         { Lex TkWhile       _ }
        --"until"         { Lex TkUntil       _ }
        --"break"         { Lex TkBreak       _ }
        --"continue"      { Lex TkContinue    _ }

        -- Expressions/Operators
        -- -- Literals
        --int             { Lex (TkInt    $$) _ }
        --"true"          { Lex (TkTrue   $$) _ }
        --"false"         { Lex (TkFalse  $$) _ }
        --float           { Lex (TkFloat  $$) _ }
        --string          { Lex (TkString $$) _ }
        --char            { Lex (TkChar   $$) _ }

        -- -- Num
        --"+"             { Lex TkPlus        _ }
        --"-"             { Lex TkMinus       _ }
        --"*"             { Lex TkTimes       _ }
        --"/"             { Lex TkDivide      _ }
        --"%"             { Lex TkModulo      _ }
        --"^"             { Lex TkPower       _ }

        -- -- Bool
        --"or"            { Lex TkOr          _ }
        --"and"           { Lex TkAnd         _ }
        --"not"           { Lex TkNot         _ }
        --"@"             { Lex TkBelongs     _ }
        --"=="            { Lex TkEqual       _ }
        --"/="            { Lex TkUnequal     _ }
        --"<"             { Lex TkLess        _ }
        --">"             { Lex TkGreat       _ }
        --"<="            { Lex TkLessEq      _ }
        --">="            { Lex TkGreatEq     _ }

        -- -- Identifiers
        --varid           { Lex (TkVarId  $$) _ }
        --typeid          { Lex (TkTypeId $$) _ }



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
        int             { TkInt    $$  }
        "true"          { TkTrue   $$  }
        "false"         { TkFalse  $$  }
        float           { TkFloat  $$  }
        string          { TkString $$  }
        char            { TkChar   $$  }

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
        "@"             { TkBelongs    }
        "=="            { TkEqual      }
        "/="            { TkUnequal    }
        "<"             { TkLess       }
        ">"             { TkGreat      }
        "<="            { TkLessEq     }
        ">="            { TkGreatEq    }

        -- -- Identifiers
        varid           { TkVarId  $$  }
        typeid          { TkTypeId $$  }

--------------------------------------------------------------------------------
-- Precedence
-- -- Language
%left ","

-- Bool
%left "or"
%left "and"
%right "not"

-- -- Compare
%nonassoc "@"
%nonassoc "==" "/="
%nonassoc "<" "<=" ">" ">="

-- Arithmetic
%left "+" "-"
%left "*" "/" "%"
%left ".."
%right "-"
%right "^"

%%

--------------------------------------------------------------------------------
-- Grammar

Program :: { Program }
    : StatementList         { Program $1 }
--    | error                 { [parseError StNoop "Expecting a statement"] }

StatementList :: { Seq Statement }
    : Statement                             { singleton $1 }
    | StatementList Separator Statement     { $1 |> $3 }
--    | error                                 { [parseError StNoop "Expecting a statement"] }

Statement :: { Statement }
    :                           { StNoop }      -- λ, no-operation
    | varid "=" Expression      { StAssign $1 $3 }

    -- Definitions
    | DataType VariableList     { StDeclaration $ fmap (\var -> Declaration var $1 CatVariable) $2 }
--    | FunctionDef               { {- NI IDEA -} }
    | "return" Expression       { StReturn $2 }

    -- Conditional
    | "if" Expression "then" StatementList "end"                           { StIf $2           $4 empty }
    | "if" Expression "then" StatementList "else" StatementList "end"      { StIf $2           $4 $6    }
    | "unless" Expression "then" StatementList "end"                       { StIf (ExpUnary OpNot $2) $4 empty }
    | "unless" Expression "then" StatementList "else" StatementList "end"  { StIf (ExpUnary OpNot $2) $4 $6    }
--    | "case" ExpressionArit CaseList "end"                                     { StCase $2 $3 empty         }   
--    | "case" ExpressionArit CaseList "else" StatementList "end"                { StCase $2 $3 $5            }   

    -- I/O
    | "read" VariableList       { StRead  $2 }
    | "print" ExpressionList    { StPrint $2 }

    -- Loops
    | "while" Expression "do" StatementList "end"          { StWhile $2           $4 }
    | "until" Expression "do" StatementList "end"          { StWhile (ExpUnary OpNot $2) $4 }

--    | "repeat" StatementList "while" ExpressionBool            { StRepeat $2 $4           }
--    | "repeat" StatementList "until" ExpressionBool            { StRepeat $2 (ExpUnary OpNot $4) }

--    | "for" varid "in" ExpressionRang "do" StatementList "end" { StFor $2 $4 $6          }
    | "break"           { StBreak }
    | "continue"        { StContinue }
--    | error         { parseError StNoop "Expecting a statement" }

Separator :: { () }
    : ";"           {}
    | newline       {}

CaseList --:: { Seq Case }
    : Case              { singleton $1 }
    | CaseList Case     { $1 |> $2     }

Case :: { Case }
    : "when" Expression "do" StatementList      { Case $2 $4 }

---------------------------------------

DataType :: { DataType }
    : "Int"         { Int }
    | "Float"       { Float }
    | "Bool"        { Bool }
    | "Char"        { Char }
    | "String"      { String }
    | "Range"       { Range }
    | "Type"        { Type }
--    | "Union" typeid
--    | "Record" typeid --            
------------------------------ FALTA ARREGLOS --
--DataTypeArray ----    : "[" DataType "]" "<-" "[" int "]" 

VariableList :: { Seq Identifier }
             : varid                         { singleton $1 }
             | VariableList "," varid        { $1 |> $3 } 
             
--FunctionDef :: { Function } 
--    : "def" varid "::" Signature
--    | "def" varid "(" VariableList ")" "::" Signature "as" StatementList "end" -- length(ParemeterList) == length(Signature) - 1

--Signature --:: { Signature }
--    : DataType
--    | Signature "->" DataType

---------------------------------------

Expression :: { Expression }
    -- Variable
    : varid                         { Variable $1 }
    -- Literals
    | int                           { LitInt $1    }
    | float                         { LitFloat $1  }
    | "true"                        { LitBool $1   }
    | "false"                       { LitBool $1   }
    | char                          { LitChar $1   }
    | string                        { LitString $1 }
    -- Operators
    | Expression "+"   Expression  { ExpBinary OpPlus    $1 $3 {-Void-} }
    | Expression "-"   Expression  { ExpBinary OpMinus   $1 $3 {-Void-} }
    | Expression "*"   Expression  { ExpBinary OpTimes   $1 $3 {-Void-} }
    | Expression "/"   Expression  { ExpBinary OpDivide  $1 $3 {-Void-} }
    | Expression "%"   Expression  { ExpBinary OpModulo  $1 $3 {-Void-} }
    | Expression "^"   Expression  { ExpBinary OpPower   $1 $3 {-Void-} }
    | Expression ".."  Expression  { ExpBinary OpFromTo  $1 $3 {-Void-} }
    | Expression "or"  Expression  { ExpBinary OpOr      $1 $3 {-Void-} }
    | Expression "and" Expression  { ExpBinary OpAnd     $1 $3 {-Void-} }
    | Expression "=="  Expression  { ExpBinary OpEqual   $1 $3 {-Void-} }
    | Expression "/="  Expression  { ExpBinary OpUnEqual $1 $3 {-Void-} }
    | Expression "<"   Expression  { ExpBinary OpLess    $1 $3 {-Void-} }
    | Expression "<="  Expression  { ExpBinary OpLessEq  $1 $3 {-Void-} }
    | Expression ">"   Expression  { ExpBinary OpGreat   $1 $3 {-Void-} }
    | Expression ">="  Expression  { ExpBinary OpGreatEq $1 $3 {-Void-} }
    | Expression "@"   Expression  { ExpBinary OpBelongs $1 $3 {-Void-} }
    | "-"   Expression             { ExpUnary OpNegate $2 {-Void-} }
    | "not" Expression             { ExpUnary OpNot    $2 {-Void-} }
--    | error                         { parseError (ExpError Void) "Expecting an expression" }

ExpressionList :: { Seq Expression }
    : Expression                                { singleton $1 }
    | ExpressionList "," Expression       { $1 |> $3     }
--    | error                                     { [parseError (ExpError Void) "Expecting an expression"] }

{

--------------------------------------------------------------------------------
-- Functions

--expError :: DataType -> String -> Checker Expression
--expError dt str = do
--    tell [SError $ StaticError str]
--    return $ ExpError dt

--parseError :: a -> String -> Checker a
--parseError identity str = do
--    tell [PError $ UnexpectedToken str]
--    return identity

--------------------------------------------------------------------------------

--lexWrap :: (Lexeme -> Alex a) -> Alex a
lexWrap :: (Token -> Alex a) -> Alex a
lexWrap cont = do
    t <- alexMonadScan
    case t of
        --Lex (TkError c) p -> do
        TkError c -> do
            p <- alexGetPosn
            --fail $ alexShowPosn p ++ "Unexpected character: '" ++ [c] ++ "'\n"
            addLexerError t
            lexWrap cont
        TkStringError str -> do
            p <- alexGetPosn
            --fail $ alexShowPosn p ++ "Missing matching '\"' for string\n"
            addLexerError t
            lexWrap cont
        _         -> cont t

--happyError :: Lexeme -> Alex a
--happyError (Lex t p) = fail $ alexShowPosn p ++ "Parse error on Token: " ++ show t ++ "\n"
happyError :: Token -> Alex a
happyError t = do
    p <- alexGetPosn
    fail $ alexShowPosn p ++ "Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> Either [(LexerError, Lexeme)] Program
parseProgram input = runAlex' input parse

}
