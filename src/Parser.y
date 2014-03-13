{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Checker

import           Prelude
import           Data.List (foldl')
import qualified Data.Foldable as DF
import           Data.Sequence
--import           Control.Monad.RWS
--import           Control.Monad
}

%name parse
%tokentype { Lexeme Token }
--%tokentype { Token }
%monad { Alex }
%lexer { lexWrap } { Lex TkEOF _ }
--%lexer { lexWrap } { TkEOF }
-- Without this we get a type error
%error { happyError }

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

StatementList :: { Seq (Lexeme Statement) }
    : Statement                             { singleton $1 }
    | StatementList Separator Statement     { $1 |> $3     }

Statement :: { Lexeme Statement }
    :                           { Lex StNoop (0,0) }      -- λ, no-operation
    | varid "=" Expression      { liftLex $1 $ \(TkVarId v) -> StAssign (putLex $1 v) $3 }

--    -- Definitions
    | DataType VariableList     { putLex $1 . StDeclaration $ fmap (\var -> putLex $1 $ Declaration var $1 CatVariable) $2 }
    --| DataType VariableList     { putLex $1 . StDeclaration $ foldl' (\r var -> (putLex $1 $ Declaration var $1 CatVariable) : r) [] $2 }
--    | FunctionDef               { {- NI IDEA -} }
--    | "return" Expression       { StReturn $2 }

--    -- Conditional
------------------------------    --| "if" Expression "then" StatementList "end"                           { StIf $2           $4 empty }
------------------------------    --| "if" Expression "then" StatementList "else" StatementList "end"      { StIf $2           $4 $6    }
------------------------------    --| "unless" Expression "then" StatementList "end"                       { StIf (ExpUnary OpNot $2) $4 empty }
------------------------------    --| "unless" Expression "then" StatementList "else" StatementList "end"  { StIf (ExpUnary OpNot $2) $4 $6    }
--    | "case" ExpressionArit CaseList "end"                                     { StCase $2 $3 empty         }
--    | "case" ExpressionArit CaseList "else" StatementList "end"                { StCase $2 $3 $5            }

--    -- I/O
------------------------------    --| "read" VariableList       { StRead  $2 }
------------------------------    --| "print" ExpressionList    { StPrint $2 }

--    -- Loops
------------------------------    --| "while" Expression "do" StatementList "end"          { StWhile $2           $4 }
------------------------------    --| "until" Expression "do" StatementList "end"          { StWhile (ExpUnary OpNot $2) $4 }

--    | "repeat" StatementList "while" ExpressionBool            { StRepeat $2 $4           }
--    | "repeat" StatementList "until" ExpressionBool            { StRepeat $2 (ExpUnary OpNot $4) }

--    | "for" varid "in" ExpressionRang "do" StatementList "end" { StFor $2 $4 $6          }
------------------------------    --| "break"           { StBreak }
------------------------------    --| "continue"        { StContinue }

Separator :: { () }
    : ";"           { }
    | newline       { }

CaseList :: { Seq Case }
    : Case              { singleton $1 }
    | CaseList Case     { $1 |> $2     }

Case :: { Case }
    : "when" Expression "do" StatementList      { Case $2 $4 }

---------------------------------------

DataType :: { Lexeme DataType }
    : "Int"         { putLex $1 Int    }
    | "Float"       { putLex $1 Float  }
    | "Bool"        { putLex $1 Bool   }
    | "Char"        { putLex $1 Char   }
    | "String"      { putLex $1 String }
    | "Range"       { putLex $1 Range  }
    | "Type"        { putLex $1 Type   }
--    | "Union" typeid
--    | "Record" typeid
-------------------------------- FALTA ARREGLOS

----DataTypeArray
----    : "[" DataType "]" "<-" "[" int "]"

VariableList :: { Seq (Lexeme Identifier) }
    : varid                         { singleton (liftLex $1 $ \(TkVarId v) -> v) }
    | VariableList "," varid        { $1 |> (liftLex $3 $ \(TkVarId v) -> v)     }

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
    | Expression "/="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpUnequal) $1 $3 }
    | Expression "<"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpLess   ) $1 $3 }
    | Expression "<="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpLessEq ) $1 $3 }
    | Expression ">"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpGreat  ) $1 $3 }
    | Expression ">="  Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpGreatEq) $1 $3 }
    | Expression "@"   Expression  { putLex $1 $ ExpBinary (liftLex $2 $ const OpBelongs) $1 $3 }
    | "-"   Expression             { putLex $1 $ ExpUnary (liftLex $2 (const OpNegate)) $2 }
    | "not" Expression             { putLex $1 $ ExpUnary (liftLex $2 (const OpNot   )) $2 }

ExpressionList :: { Seq (Lexeme Expression) }
    : Expression                          { singleton $1 }
    | ExpressionList "," Expression       { $1 |> $3     }

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

liftPutLex :: Lexeme a -> (a -> b) -> (Lexeme b -> c) -> Lexeme c
liftPutLex a f g = putLex a . g $ liftLex a f

liftLex :: Lexeme a -> (a -> b) -> Lexeme b
liftLex (Lex t p) f = Lex (f t) p

putLex :: Lexeme a -> b -> Lexeme b
putLex l a = liftLex l $ const a

----------------------------------------

--lexWrap :: (Token -> Alex a) -> Alex a
lexWrap :: (Lexeme Token -> Alex a) -> Alex a
lexWrap cont = do
    t <- alexMonadScan
    case t of
        Lex (TkError c) p -> do
            p <- alexGetPosn
            addLexerError t
            lexWrap cont
        Lex (TkStringError str) p -> do
            p <- alexGetPosn
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
