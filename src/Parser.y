{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Checker

import           Prelude
import qualified Data.Foldable as DF
import           Data.Functor
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
        "true"          { Lex (TkBool   _)  _ }
        "false"         { Lex (TkBool   _)  _ }
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

-- -- Bool
%left "or"
%left "and"
%right "not"

-- -- -- Compare
%nonassoc "@"
%nonassoc "==" "/="
%nonassoc "<" "<=" ">" ">="

-- -- Arithmetic
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
    | varid "=" Expression      { (flip StAssign $3 . (<$ $1) . unTkVarId) `fmap` $1 }

    -- Definitions
    | DataType VariableList     { (StDeclaration $ fmap (\lVar -> (Declaration lVar $1 CatVariable) <$ lVar) $2) <$ $1 }
    | FunctionDef               { $1 }
--    | "return" Expression       { StReturn $2 }

    -- Conditional
    | "if" Expression "then" StatementList "end"                            { StIf $2 $4 empty <$ $1 }
    | "if" Expression "then" StatementList "else" StatementList "end"       { StIf $2 $4 $6    <$ $1 }
    | "unless" Expression "then" StatementList "end"                        { StIf ((ExpUnary (OpNot <$ $2) $2) <$ $2) $4 empty <$ $1 }
    | "unless" Expression "then" StatementList "else" StatementList "end"   { StIf ((ExpUnary (OpNot <$ $2) $2) <$ $2) $4 $6    <$ $1 }
--    | "case" ExpressionArit CaseList "end"                                  { StCase $2 $3 empty         }
--    | "case" ExpressionArit CaseList "else" StatementList "end"             { StCase $2 $3 $5            }

    -- I/O
    | "read" VariableList       { StRead  $2 <$ $1 }
    | "print" ExpressionList    { StPrint $2 <$ $1 }

    -- Loops
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
    : "Int"         { Int    <$ $1 }
    | "Float"       { Float  <$ $1 }
    | "Bool"        { Bool   <$ $1 }
    | "Char"        { Char   <$ $1 }
    | "String"      { String <$ $1 }
    | "Range"       { Range  <$ $1 }
    | "Type"        { Type   <$ $1 }
--    | "Union" typeid
--    | "Record" typeid

---- ARREGLOS
----DataTypeArray
----    : "[" DataType "]" "<-" "[" int "]"

VariableList :: { Seq (Lexeme Identifier) }
    : varid                         { singleton $ unTkVarId `fmap` $1  }
    | VariableList "," varid        { $1      |> (unTkVarId `fmap` $3) }

FunctionDef :: { Lexeme Statement }
    : "def" varid "::" Signature                                               { (StFunction (Just $4) empty) <$ $1 } -- Definir solo la firma sin la lista de parametros
    | "def" varid "(" VariableList ")" "as" StatementList "end"                { (StFunction Nothing   $7)    <$ $1 } -- Implementar la funcion (debe haberse definido anteriormente)
    | "def" varid "(" VariableList ")" "::" Signature "as" StatementList "end" { (StFunction (Just $7) $9)    <$ $1 } -- Definir e implementar la funcion al mismo tiempo

Signature :: { Lexeme Signature }
    : "(" DataTypeList ")" FunctionReturn { (Signature $2             $4) <$ $1 }
    | DataType FunctionReturn             { (Signature (singleton $1) $2) <$ $1 }

DataTypeList :: { Seq (Lexeme DataType) }
    : DataType                   { singleton $1 }
    | DataTypeList "," DataType  { $1 |> $3     }
    
FunctionReturn :: { Lexeme DataType }
    : "->" DataType                { $2 } 
    |                              { Lex Void (0,0) }

---------------------------------------

Expression :: { Lexeme Expression }
    -- Variable
    : varid                        { Variable (unTkVarId `fmap` $1) <$ $1 }
    -- Literals
    | int                          { LitInt    (unTkInt    `fmap` $1) <$ $1 }
    | float                        { LitFloat  (unTkFloat  `fmap` $1) <$ $1 }
    | string                       { LitString (unTkString `fmap` $1) <$ $1 }
    | char                         { LitChar   (unTkChar   `fmap` $1) <$ $1 }
    | "true"                       { LitBool   (unTkBool   `fmap` $1) <$ $1 }
    | "false"                      { LitBool   (unTkBool   `fmap` $1) <$ $1 }
    -- Operators
    | Expression "+"   Expression  { ExpBinary (OpPlus    <$ $2) $1 $3 <$ $1 }
    | Expression "-"   Expression  { ExpBinary (OpMinus   <$ $2) $1 $3 <$ $1 }
    | Expression "*"   Expression  { ExpBinary (OpTimes   <$ $2) $1 $3 <$ $1 }
    | Expression "/"   Expression  { ExpBinary (OpDivide  <$ $2) $1 $3 <$ $1 }
    | Expression "%"   Expression  { ExpBinary (OpModulo  <$ $2) $1 $3 <$ $1 }
    | Expression "^"   Expression  { ExpBinary (OpPower   <$ $2) $1 $3 <$ $1 }
    | Expression ".."  Expression  { ExpBinary (OpFromTo  <$ $2) $1 $3 <$ $1 }
    | Expression "or"  Expression  { ExpBinary (OpOr      <$ $2) $1 $3 <$ $1 }
    | Expression "and" Expression  { ExpBinary (OpAnd     <$ $2) $1 $3 <$ $1 }
    | Expression "=="  Expression  { ExpBinary (OpEqual   <$ $2) $1 $3 <$ $1 }
    | Expression "/="  Expression  { ExpBinary (OpUnequal <$ $2) $1 $3 <$ $1 }
    | Expression "<"   Expression  { ExpBinary (OpLess    <$ $2) $1 $3 <$ $1 }
    | Expression "<="  Expression  { ExpBinary (OpLessEq  <$ $2) $1 $3 <$ $1 }
    | Expression ">"   Expression  { ExpBinary (OpGreat   <$ $2) $1 $3 <$ $1 }
    | Expression ">="  Expression  { ExpBinary (OpGreatEq <$ $2) $1 $3 <$ $1 }
    | Expression "@"   Expression  { ExpBinary (OpBelongs <$ $2) $1 $3 <$ $1 }
    | "-"   Expression             { ExpUnary  (OpNegate  <$ $1) $2    <$ $1 }
    | "not" Expression             { ExpUnary  (OpNot     <$ $1) $2    <$ $1 }
    | "(" Expression ")"           { let (Lex expr p) = $2 in expr <$ $1 }

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

parseProgram :: String -> Either [(LexerError, Lexeme Token)] Program
parseProgram input = runAlex' input parse

}
