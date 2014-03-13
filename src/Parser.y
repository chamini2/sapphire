{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Checker
--import           SymbolTable

import           Prelude
import           Data.List (foldl')
--import           Control.Monad.RWS
--import           Control.Monad
--import           Data.List (find)
}

%name parse
%tokentype { Lexeme Token }
--%tokentype { Token }
%monad { Alex }
%lexer { lexWrap } { Lex TkEOF _ }
--%lexer { lexWrap } { TkEOF }
-- Without this we get a type error
%error { happyError }

--%attributetype       { Attribute a }
--%attribute value     { a }
--%attribute data_type { DataType }
--%attribute len       { Int }

%token

        -- Language
        newline         { Lex TkNewLine     _ }
        "main"          { Lex TkMain        _ }
        "begin"         { Lex TkBegin       _ }
        "end"           { Lex TkEnd         _ }
        "return"        { Lex TkReturn      _ }
        ";"             { Lex TkSemicolon   _ }
        ","             { Lex TkComma       _ }

        -- -- Brackets
        "("             { Lex TkLParen      _ }
        ")"             { Lex TkRParen      _ }
        "["             { Lex TkLBrackets   _ }
        "]"             { Lex TkRBrackets   _ }
        "{"             { Lex TkLBraces     _ }
        "}"             { Lex TkRBraces     _ }

        -- Types
        "Void"          { Lex TkVoidType    _ }
        "Int"           { Lex TkIntType     _ }
        "Bool"          { Lex TkBoolType    _ }
        "Float"         { Lex TkFloatType   _ }
        "Char"          { Lex TkCharType    _ }
        "String"        { Lex TkStringType  _ }
        "Range"         { Lex TkRangeType   _ }
        "Union"         { Lex TkUnionType   _ }
        "Record"        { Lex TkRecordType  _ }
        "Type"          { Lex TkTypeType    _ }

        -- Statements
        -- -- Declarations
        "="             { Lex TkAssign      _ }
        "def"           { Lex TkDef         _ }
        "as"            { Lex TkAs          _ }
        "::"            { Lex TkSignature   _ }
        "->"            { Lex TkArrow       _ }

        -- -- In/Out
        "read"          { Lex TkRead        _ }
        "print"         { Lex TkPrint       _ }

        -- -- Conditionals
        "if"            { Lex TkIf          _ }
        "then"          { Lex TkThen        _ }
        "else"          { Lex TkElse        _ }
        "unless"        { Lex TkUnless      _ }
        "case"          { Lex TkCase        _ }
        "when"          { Lex TkWhen        _ }

        -- -- Loops
        "for"           { Lex TkFor         _ }
        "in"            { Lex TkIn          _ }
        ".."            { Lex TkFromTo      _ }
        "do"            { Lex TkDo          _ }
        "while"         { Lex TkWhile       _ }
        "until"         { Lex TkUntil       _ }
        "break"         { Lex TkBreak       _ }
        "continue"      { Lex TkContinue    _ }

        -- Expressions/Operators
        -- -- Literals
        int             { Lex (TkInt    _)  _ }
        "true"          { Lex (TkTrue   _)  _ }
        "false"         { Lex (TkFalse  _)  _ }
        float           { Lex (TkFloat  _)  _ }
        string          { Lex (TkString _)  _ }
        char            { Lex (TkChar   _)  _ }

        -- -- Num
        "+"             { Lex TkPlus        _ }
        "-"             { Lex TkMinus       _ }
        "*"             { Lex TkTimes       _ }
        "/"             { Lex TkDivide      _ }
        "%"             { Lex TkModulo      _ }
        "^"             { Lex TkPower       _ }

        -- -- Bool
        "or"            { Lex TkOr          _ }
        "and"           { Lex TkAnd         _ }
        "not"           { Lex TkNot         _ }
        "@"             { Lex TkBelongs     _ }
        "=="            { Lex TkEqual       _ }
        "/="            { Lex TkUnequal     _ }
        "<"             { Lex TkLess        _ }
        ">"             { Lex TkGreat       _ }
        "<="            { Lex TkLessEq      _ }
        ">="            { Lex TkGreatEq     _ }

        -- -- Identifiers
        varid           { Lex (TkVarId  _)  _ }
        typeid          { Lex (TkTypeId _)  _ }



        -- Language
        --newline         { TkNewLine    }
        --"main"          { TkMain       }
        --"begin"         { TkBegin      }
        --"end"           { TkEnd        }
        --"return"        { TkReturn     }
        --";"             { TkSemicolon  }
        --","             { TkComma      }

        -- -- Brackets
        --"("             { TkLParen     }
        --")"             { TkRParen     }
        --"["             { TkLBrackets  }
        --"]"             { TkRBrackets  }
        --"{"             { TkLBraces    }
        --"}"             { TkRBraces    }

        -- Types
        --"Void"          { TkVoidType   }
        --"Int"           { TkIntType    }
        --"Bool"          { TkBoolType   }
        --"Float"         { TkFloatType  }
        --"Char"          { TkCharType   }
        --"String"        { TkStringType }
        --"Range"         { TkRangeType  }
        --"Union"         { TkUnionType  }
        --"Record"        { TkRecordType }
        --"Type"          { TkTypeType   }

        -- Statements
        -- -- Declarations
        --"="             { TkAssign     }
        --"def"           { TkDef        }
        --"as"            { TkAs         }
        --"::"            { TkSignature  }
        --"->"            { TkArrow      }

        -- -- In/Out
        --"read"          { TkRead       }
        --"print"         { TkPrint      }

        -- -- Conditionals
        --"if"            { TkIf         }
        --"then"          { TkThen       }
        --"else"          { TkElse       }
        --"unless"        { TkUnless     }
        --"case"          { TkCase       }
        --"when"          { TkWhen       }

        -- -- Loops
        --"for"           { TkFor        }
        --"in"            { TkIn         }
        --".."            { TkFromTo     }
        --"do"            { TkDo         }
        --"while"         { TkWhile      }
        --"until"         { TkUntil      }
        --"break"         { TkBreak      }
        --"continue"      { TkContinue   }

        -- Expressions/Operators
        -- -- Literals
        --int             { TkInt    $$  }
        --"true"          { TkTrue   $$  }
        --"false"         { TkFalse  $$  }
        --float           { TkFloat  $$  }
        --string          { TkString $$  }
        --char            { TkChar   $$  }

        -- -- Num
        --"+"             { TkPlus       }
        --"-"             { TkMinus      }
        --"*"             { TkTimes      }
        --"/"             { TkDivide     }
        --"%"             { TkModulo     }
        --"^"             { TkPower      }

        -- -- Bool
        --"or"            { TkOr         }
        --"and"           { TkAnd        }
        --"not"           { TkNot        }
        --"@"             { TkBelongs    }
        --"=="            { TkEqual      }
        --"/="            { TkUnequal    }
        --"<"             { TkLess       }
        --">"             { TkGreat      }
        --"<="            { TkLessEq     }
        --">="            { TkGreatEq    }

        -- -- Identifiers
        --varid           { TkVarId  $$  }
        --typeid          { TkTypeId $$  }

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
    : StatementList         { Program $ reverse $1 }

StatementList :: { [Lexeme Statement] }
    : Statement                             { $1 : [] }
    | StatementList Separator Statement     { $3 : $1 }

Statement :: { Lexeme Statement }
    :                           { Lex StNoop (0,0) }      -- λ, no-operation
    --| varid "=" Expression      { StAssign $1 $3 }
    | varid "=" Expression      { liftLex $1 $ \(TkVarId v) -> StAssign (putLex $1 v) $3 }

--    -- Definitions
    --| DataType VariableList     { StDeclaration $ foldl' (\r var -> (Declaration var $1 CatVariable) : r) [] $2 }
    | DataType VariableList     { putLex $1 . StDeclaration $ foldl' (\r var -> (putLex $1 $ Declaration var $1 CatVariable) : r) [] $2 }
--    | FunctionDef               { {- NI IDEA -} }
--    | "return" Expression       { StReturn $2 }

--    -- I/O
--    | "read" VariableList       { StRead  (reverse $2) }
--    | "print" ExpressionList    { StPrint (reverse $2) }

--    -- Conditional
--    | "if" ExpressionBool "then" StatementList "end"                           { StIf $2           (reverse $4) []           }
--    | "if" ExpressionBool "then" StatementList "else" StatementList "end"      { StIf $2           (reverse $4) (reverse $6) }
--    | "unless" ExpressionBool "then" StatementList "end"                       { StIf (NotBool $2) (reverse $4) []           }
--    | "unless" ExpressionBool "then" StatementList "else" StatementList "end"  { StIf (NotBool $2) (reverse $4) (reverse $6) }
--    | "case" ExpressionArit CaseList "end"                                     { StCase $2 (reverse $3) []                   }
--    | "case" ExpressionArit CaseList "else" StatementList "end"                { StCase $2 (reverse $3) (reverse $5)         }

--    -- Loops
--    | "while" ExpressionBool "do" StatementList "end"          { StWhile $2           (reverse $4) }
--    | "until" ExpressionBool "do" StatementList "end"          { StWhile (NotBool $2) (reverse $4) }

    --| "repeat" StatementList "while" ExpressionBool            { StRepeat (reverse $2) $4           }
    --| "repeat" StatementList "until" ExpressionBool            { StRepeat (reverse $2) (NotBool $4) }

--    | "for" varid "in" ExpressionRang "do" StatementList "end" { StFor $2 $4 (reverse $6)          }
--    | "break"           { StBreak }
--    | "continue"        { StContinue }

Separator :: { () }
    : ";"           { }
    | newline       { }

--CaseList --:: { [Case] }
--    : Case              { [$1]    }
--    | CaseList Case     { $2 : $1 }

--Case --:: { Case }
--    : "when" Expression "do" StatementList      { Case $2 (reverse $4) }

---------------------------------------

DataType :: { Lexeme DataType }
    : "Int"         { putLex $1 Int }
    | "Float"       { putLex $1 Float }
    | "Bool"        { putLex $1 Bool }
    | "Char"        { putLex $1 Char }
    | "String"      { putLex $1 String }
    | "Range"       { putLex $1 Range }
    | "Type"        { putLex $1 Type }
--    | "Union" typeid
--    | "Record" typeid
-------------------------------- FALTA ARREGLOS

----DataTypeArray
----    : "[" DataType "]" "<-" "[" int "]"

VariableList :: { [Lexeme Identifier] }
    : varid                         { (liftLex $1 $ \(TkVarId v) -> v) : [] }
    | VariableList "," varid        { (liftLex $3 $ \(TkVarId v) -> v) : $1 }

--FunctionDef --:: { Function }
--    : "def" varid "::" Signature
--    | "def" varid "(" VariableList ")" "::" Signature "as" StatementList "end" -- length(ParemeterList) == length(Signature) - 1

--Signature --:: { Signature }
--    : DataType
--    | Signature "->" DataType

---------------------------------------

Expression :: { Lexeme Expression }
    -- Variable
    : varid                        { liftPutLex $1 (\(TkVarId  v) -> v) Variable }
    -- Literals
    | int                          { liftPutLex $1 (\(TkInt    v) -> v) LitInt    }
    | float                        { liftPutLex $1 (\(TkFloat  v) -> v) LitFloat  }
    | string                       { liftPutLex $1 (\(TkString v) -> v) LitString }
    | char                         { liftPutLex $1 (\(TkChar   v) -> v) LitChar   }
    | "true"                       { liftPutLex $1 (\(TkTrue   v) -> v) LitBool   }
    | "false"                      { liftPutLex $1 (\(TkFalse  v) -> v) LitBool   }
    -- Operators
    --| Expression "+"   Expression  { ExpBinary OpPlus    $1 $3 {-Void-} }
    --| Expression "-"   Expression  { ExpBinary OpMinus   $1 $3 {-Void-} }
    --| Expression "*"   Expression  { ExpBinary OpTimes   $1 $3 {-Void-} }
    --| Expression "/"   Expression  { ExpBinary OpDivide  $1 $3 {-Void-} }
    --| Expression "%"   Expression  { ExpBinary OpModulo  $1 $3 {-Void-} }
    --| Expression "^"   Expression  { ExpBinary OpPower   $1 $3 {-Void-} }
    --| Expression ".."  Expression  { ExpBinary OpFromTo  $1 $3 {-Void-} }
    --| Expression "or"  Expression  { ExpBinary OpOr      $1 $3 {-Void-} }
    --| Expression "and" Expression  { ExpBinary OpAnd     $1 $3 {-Void-} }
    --| Expression "=="  Expression  { ExpBinary OpEqual   $1 $3 {-Void-} }
    --| Expression "/="  Expression  { ExpBinary OpUnEqual $1 $3 {-Void-} }
    --| Expression "<"   Expression  { ExpBinary OpLess    $1 $3 {-Void-} }
    --| Expression "<="  Expression  { ExpBinary OpLessEq  $1 $3 {-Void-} }
    --| Expression ">"   Expression  { ExpBinary OpGreat   $1 $3 {-Void-} }
    --| Expression ">="  Expression  { ExpBinary OpGreatEq $1 $3 {-Void-} }
    --| Expression "@"   Expression  { ExpBinary OpBelongs $1 $3 {-Void-} }
    | Expression "+"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpPlus   ) $1 $3 }
    | Expression "-"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpMinus  ) $1 $3 }
    | Expression "*"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpTimes  ) $1 $3 }
    | Expression "/"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpDivide ) $1 $3 }
    | Expression "%"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpModulo ) $1 $3 }
    | Expression "^"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpPower  ) $1 $3 }
    | Expression ".."  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpFromTo ) $1 $3 }
    | Expression "or"  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpOr     ) $1 $3 }
    | Expression "and" Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpAnd    ) $1 $3 }
    | Expression "=="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpEqual  ) $1 $3 }
    | Expression "/="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpUnEqual) $1 $3 }
    | Expression "<"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpLess   ) $1 $3 }
    | Expression "<="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpLessEq ) $1 $3 }
    | Expression ">"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpGreat  ) $1 $3 }
    | Expression ">="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpGreatEq) $1 $3 }
    | Expression "@"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpBelongs) $1 $3 }
    --| "-"   Expression             { ExpUnary OpNegate $2 {-Void-} }
    --| "not" Expression             { ExpUnary OpNot    $2 {-Void-} }
    | "-"   Expression             { putLex $1 $ ExpUnary (liftLex $2 (const OpNegate)) $2 }
    | "not" Expression             { putLex $1 $ ExpUnary (liftLex $2 (const OpNot   )) $2 }

ExpressionList :: { [Lexeme Expression] }
    : Expression                                { $1 : [] }
    | ExpressionList Separator Expression       { $3 : $1 }

{

--------------------------------------------------------------------------------
-- Functions

--binaryM :: Binary -> Checker Expression -> Checker Expression -> Checker Expression
--binaryM op leftM rightM = do
--    left  <- leftM
--    right <- rightM
--    let checking = checkBinaryType left right
--    case op of
--        OpOr      -> checking [(Bool,Bool,Bool)]
--        OpAnd     -> checking [(Bool,Bool,Bool)]
--        OpEqual   -> checking ((Bool,Bool,Bool) : numbers)
--        OpUnEqual -> checking ((Bool,Bool,Bool) : numbers)
--        OpFromTo  -> checking [(Int, Int, Range)]
--        OpBelongs -> checking [(Int, Range, Bool)]
--        _         -> checking numbers -- OpPlus OpMinus OpTimes OpDivide OpModulo OpPower OpLess OpLessEq OpGreat OpGreatEq
--    where
--        numbers = [(Int, Int, Int), (Float, Float, Float)]
--        checkBinaryType :: Expression -> Expression -> [(DataType,DataType,DataType)] -> Checker Expression
--        checkBinaryType left right types = do
--            let cond (l,r,_) = dataType left == l && dataType right == r
--                defaultType  = (\(_,_,r) -> r) $ head types -- We have to calculate better the defaultType
--            case find cond types of
--                Just (_,_,r) -> return $ ExpBinary op left right r
--                Nothing      -> expError defaultType $ "Static Error: operator " ++ show op ++ " doesn't work with arguments " ++
--                                           show (dataType left) ++ ", " ++ show (dataType right) ++ "\n"

--unaryM :: Unary -> Checker Expression -> Checker Expression
--unaryM op operandM = do
--    operand <- operandM
--    let checking = checkUnaryType operand
--    case op of
--        OpNegate -> checking [(Int, Int), (Float, Float)]
--        OpNot    -> checking [(Bool,Bool)]
--    where
--        checkUnaryType :: Expression -> [(DataType,DataType)] -> Checker Expression
--        checkUnaryType operand types = do
--            let cond (u,_)  = dataType operand == u
--                defaultType = snd $ head types -- We have to calculate better the defaultType
--            case find cond types of
--                Just (_,r) -> return $ ExpUnary op operand r
--                Nothing    -> expError defaultType $ "Static Error: operator " ++ show op ++ "doesn't work with arguments " ++
--                                         show (dataType operand) ++ "\n"


--expError :: DataType -> String -> Checker Expression
--expError dt str = do
--    tell [SError $ StaticError str]
--    return $ ExpError dt

--parseError :: a -> String -> Checker a
--parseError identity str = do
--    tell [PError $ UnexpectedToken str]
--    return identity

--continueChecker :: Checker a -> (a -> b) -> Checker b
--continueChecker extract func = do
--    let (check, state, writer) = runChecker extract
--    tell writer
--    put state
--    return $ func check


--------------------------------------------------------------------------------

liftPutLex :: Lexeme a -> (a -> b) -> (Lexeme b -> c) -> Lexeme c
liftPutLex a f g = putLex a . g $ liftLex a f

liftLex :: Lexeme a -> (a -> b) -> Lexeme b
liftLex (Lex t p) f = Lex (f t) p

putLex :: Lexeme a -> b -> Lexeme b
putLex l a = liftLex l $ const a


--unLex :: Lexeme -> (Token -> a) -> Posn a
--unLex (Lex t p) f = Posn (f t) p

--positionPosn :: Posn b -> a -> Posn a
--positionPosn (Posn _ p) a = Posn a p

--positionLex :: Lexeme -> a -> Posn a
--positionLex l a = unLex l $ const a


lexWrap :: (Lexeme Token -> Alex a) -> Alex a
--lexWrap :: (Token -> Alex a) -> Alex a
lexWrap cont = do
    t <- alexMonadScan
    case t of
        Lex (TkError c) p -> do
        --TkError c -> do
            p <- alexGetPosn
            --fail $ showPosn p ++ "Unexpected character: '" ++ [c] ++ "'\n"
            addLexerError t
            lexWrap cont
        Lex (TkStringError str) p -> do
        --TkStringError str -> do
            p <- alexGetPosn
            --fail $ showPosn p ++ "Missing matching '\"' for string\n"
            addLexerError t
            lexWrap cont
        _         -> cont t

happyError :: Lexeme Token -> Alex a
happyError (Lex t p) = fail $ showPosn p ++ "Parse error on Token: " ++ show t ++ "\n"
--happyError :: Token -> Alex a
--happyError t = do
--    p <- alexGetPosn
--    fail $ showPosn p ++ "Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> Either [(LexerError, Lexeme Token)] Program
parseProgram input = runAlex' input parse

}
