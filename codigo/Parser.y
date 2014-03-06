{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Language
import           Lexer

import           Control.Monad.RWS
import           Data.List (find)
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexWrap } { TkEOF }
-- Without this we get a type error
%error { happyError }

--%attributetype { Attribute a }
--%attribute value        { a }
--%attribute data_type    { DataType }
--%attribute len          { Int }

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
    : StatementList         { continueChecker $1 reverse }
--    | error                 { [parseError StNoop "Expecting a statement"] }

StatementList :: { Checker [Statement] }
    : Statement                             { continueChecker $1 (:[]) }
    | StatementList Separator Statement     { $1 >>= \ls -> continueChecker $3 (:ls) }
        -- Equivalente a
        --{ do
        --    ls <- $1
        --    continueChecker $3 (:ls)
        --}
--    | error                                 { [parseError StNoop "Expecting a statement"] }

Statement :: { Checker Statement }
    :                           { return StNoop }      -- λ, no-operation
    | varid "=" Expression      { continueChecker $3 (StAssign $1) }

--    -- Definitions
--    | DataType VariableList     { StDeclaration $ map (\var -> Declaration var $1) $2 }
--    | FunctionDef               { {- NI IDEA -} }
--    | "return" Expression       { StReturn $2 }

--    -- I/O
--    | "read" VariableList       { StRead  (reverse $2) }
--    | "print" ExpressionList    { StPrint (reverse $2) }

--    -- Conditional
--    | "if" ExpressionBool "then" StatementList "end"                          { StIf $2           (reverse $4) []           }
--    | "if" ExpressionBool "then" StatementList "else" StatementList "end"     { StIf $2           (reverse $4) (reverse $6) }
--    | "unless" ExpressionBool "then" StatementList "end"                      { StIf (NotBool $2) (reverse $4) []           }
--    | "unless" ExpressionBool "then" StatementList "else" StatementList "end" { StIf (NotBool $2) (reverse $4) (reverse $6) }
--    | "case" ExpressionArit CaseList "end"                                    { StCase $2 (reverse $3) []                   }
--    | "case" ExpressionArit CaseList "else" StatementList "end"               { StCase $2 (reverse $3) (reverse $5)         }

--    -- Loops
--    | "while" ExpressionBool "do" StatementList "end"          { StWhile $2           (reverse $4) }
--    | "until" ExpressionBool "do" StatementList "end"          { StWhile (NotBool $2) (reverse $4) }
--    | "for" varid "in" ExpressionRang "do" StatementList "end" { StFor $2 $4 (reverse $6)          }
--    | "break"           { StBreak }
--    | "continue"        { StContinue }
--    | error         { parseError StNoop "Expecting a statement" }

Separator
    : ";"           {}
    | newline       {}

--CaseList :: { [Case] }
--    : Case              { [$1]    }
--    | CaseList Case     { $2 : $1 }

--Case :: { Case }
--    : "when" Expression "do" StatementList      { Case $2 (reverse $4) }

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
--    | "Record" typeid
--            ------------------------------ FALTA ARREGLOS

----DataTypeArray
----    : "[" DataType "]" "<-" "[" int "]"

--VariableList :: { [Variable] }
--    : varid                         { [$1]    }
--    | VariableList "," varid        { $3 : $1 }

--FunctionDef :: { Function }
--    : "def" varid "::" Signature
--    | "def" varid "(" VariableList ")" "::" Signature "as" StatementList "end" -- length(ParemeterList) == length(Signature) - 1

--Signature :: { Signature }
--    : DataType
--    | Signature "->" DataType

---------------------------------------

Binary :: { Binary }
    : "+"       { OpPlus    }
    | "-"       { OpMinus   }
    | "*"       { OpTimes   }
    | "/"       { OpDivide  }
    | "%"       { OpModulo  }
    | "^"       { OpPower   }
    | ".."      { OpFromTo  }
    | "or"      { OpOr      }
    | "and"     { OpAnd     }
    | "=="      { OpEqual   }
    | "/="      { OpUnEqual }
    | "<"       { OpLess    }
    | "<="      { OpLessEq  }
    | ">"       { OpGreat   }
    | ">="      { OpGreatEq }
    | "@"       { OpBelongs }

Unary :: { Unary }
    : "-"       { OpNegate }
    | "not"     { OpNot    }


Expression :: { Checker Expression }
    -- Variable
    : varid                         { return $ Variable $1 }
    -- Literals
    | int                           { return $ LitInt $1    }
    | float                         { return $ LitFloat $1  }
    | "true"                        { return $ LitBool $1   }
    | "false"                       { return $ LitBool $1   }
    | char                          { return $ LitChar $1   }
    | string                        { return $ LitString $1 }
    -- Operators
    | Expression Binary Expression  { binaryM $2 $1 $3 }
    | Unary Expression              { unaryM $1 $2     }
--    | error                         { parseError (ExpError Void) "Expecting an expression" }

ExpressionList :: { [Checker Expression] }
    : Expression                                { [$1]    }
    | ExpressionList Separator Expression       { $3 : $1 }
--    | error                                     { [parseError (ExpError Void) "Expecting an expression"] }

{

--------------------------------------------------------------------------------
-- Functions

binaryM :: Binary -> Checker Expression -> Checker Expression -> Checker Expression
binaryM op leftM rightM = do
    left  <- leftM
    right <- rightM
    let checking = checkBinaryType left right
    case op of
        OpOr      -> checking [(Bool,Bool,Bool)]
        OpAnd     -> checking [(Bool,Bool,Bool)]
        OpEqual   -> checking ((Bool,Bool,Bool) : numbers)
        OpUnEqual -> checking ((Bool,Bool,Bool) : numbers)
        OpFromTo  -> checking [(Int, Int, Range)]
        OpBelongs -> checking [(Int, Range, Bool)]
        _         -> checking numbers -- OpPlus OpMinus OpTimes OpDivide OpModulo OpPower OpLess OpLessEq OpGreat OpGreatEq
    where
        numbers = [(Int, Int, Int), (Float, Float, Float)]
        checkBinaryType :: Expression -> Expression -> [(DataType,DataType,DataType)] -> Checker Expression
        checkBinaryType left right types = do
            let cond (l,r,_) = dataType left == l && dataType right == r
                defaultType  = (\(_,_,r) -> r) $ head types -- We have to calculate better the defaultType
            case find cond types of
                Just (_,_,r) -> return $ ExpBinary op left right r
                Nothing      -> expError defaultType $ "Static Error: operator " ++ show op ++ " doesn't work with arguments " ++
                                           show (dataType left) ++ ", " ++ show (dataType right) ++ "\n"

unaryM :: Unary -> Checker Expression -> Checker Expression
unaryM op operandM = do
    operand <- operandM
    let checking = checkUnaryType operand
    case op of
        OpNegate -> checking [(Int, Int), (Float, Float)]
        OpNot    -> checking [(Bool,Bool)]
    where
        checkUnaryType :: Expression -> [(DataType,DataType)] -> Checker Expression
        checkUnaryType operand types = do
            let cond (u,_)  = dataType operand == u
                defaultType = snd $ head types -- We have to calculate better the defaultType
            case find cond types of
                Just (_,r) -> return $ ExpUnary op operand r
                Nothing    -> expError defaultType $ "Static Error: operator " ++ show op ++ "doesn't work with arguments " ++
                                         show (dataType operand) ++ "\n"


expError :: DataType -> String -> Checker Expression
expError dt str = do
    tell [SError $ StaticError str]
    return $ ExpError dt

parseError :: a -> String -> Checker a
parseError identity str = do
    tell [PError $ UnexpectedToken str]
    return identity

continueChecker :: Checker a -> (a -> b) -> Checker b
continueChecker extract func = do
    let (check, state, writer) = runChecker extract
    tell writer
    put state
    return $ func check


--------------------------------------------------------------------------------

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap cont = do
    t <- alexMonadScan
    case t of
        TkError c -> do
            (p,_,_,_) <- alexGetInput
            fail $ showPosn p ++ "Unexpected character: '" ++ [c] ++ "'\n"
        _         -> cont t

happyError :: Token -> Alex a
happyError t = do
    (p,_,_,_) <- alexGetInput
    fail $ showPosn p ++ "Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> Either String Program
parseProgram s = runAlex s parse

}
