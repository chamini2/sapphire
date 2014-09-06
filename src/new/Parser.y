{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Checker

import           Control.Arrow (first)
import           Data.Foldable as DF (concatMap, foldr)
import           Data.Functor
import           Data.Sequence hiding (reverse, length)
import           Prelude       hiding (concatMap, foldr)
}

%name parse
%tokentype { Lexeme Token }
%monad { Alex }
%lexer { lexWrap } { Lex TkEOF _ }
%error { happyError }

%token


    -- Language
    newline         { Lex TkNewLine     _ }
    "end"           { Lex TkEnd         _ }
    "return"        { Lex TkReturn      _ }
    ";"             { Lex TkSemicolon   _ }
    ","             { Lex TkComma       _ }

    -- -- Brackets
    "("             { Lex TkLParen      _ }
    ")"             { Lex TkRParen      _ }
    "["             { Lex TkLBrackets   _ }
    "]"             { Lex TkRBrackets   _ }
    --"{"             { Lex TkLBraces     _ }
    --"}"             { Lex TkRBraces     _ }

    -- Types
    --"Void"          { Lex TkVoidType    _ }
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
    "imp"           { Lex TkImp         _ }
    "as"            { Lex TkAs          _ }
    "::"            { Lex TkSignature   _ }
    "->"            { Lex TkArrow       _ }
    "."             { Lex TkDot         _ }

    -- -- In/Out
    "read"          { Lex TkRead        _ }
    "print"         { Lex TkPrint       _ }

    -- -- Conditionals
    "if"            { Lex TkIf          _ }
    "then"          { Lex TkThen        _ }
    "elif"          { Lex TkElif        _ }
    "else"          { Lex TkElse        _ }
    "unless"        { Lex TkUnless      _ }
    "case"          { Lex TkCase        _ }
    "when"          { Lex TkWhen        _ }
    "otherwise"     { Lex TkOtherwise   _ }

    -- -- Loops
    "for"           { Lex TkFor         _ }
    "in"            { Lex TkIn          _ }
    ".."            { Lex TkFromTo      _ }
    "do"            { Lex TkDo          _ }
    "while"         { Lex TkWhile       _ }
    "until"         { Lex TkUntil       _ }
    "repeat"        { Lex TkRepeat      _ }
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
%right ","

-- -- String
--%right "++"

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

StatementList :: { StBlock }
    : Statement                             { filterSt $1       }
    | StatementList Separator Statement     { $1 >< filterSt $3 }

Separator :: { () }
    : ";"           { }
    | newline       { }

MaybeNL :: { () }
    :                      { }
    | MaybeNL newline      { }

Statement :: { Lexeme Statement }
    : {- λ, no-operation -}     { Lex StNoop (0,0) }
    --| Access "=" Expression     { StAssign $1 $3 <$ $1 }

    -- Definitions
    --| "Record" TypeId "as" MaybeNL FieldList MaybeNL "end"      { StStructDefinition (Record $2 $5 0 <$ $1) <$ $1 }
    | "Record" TypeId "as" MaybeNL FieldList MaybeNL "end"        { undefined }
    --| "Union"  TypeId "as" MaybeNL FieldList MaybeNL "end"      { StStructDefinition (Union  $2 $5 0 <$ $1) <$ $1 }
    | "Union" TypeId "as" MaybeNL FieldList MaybeNL "end"         { undefined }
    --| DataType DeclareVariableList       { (StDeclarationList $ fmap (first (\iden -> Declaration iden $1 CatVariable <$ iden)) $2) <$ $1 }
    | VariableList ":" DataType         { undefined }

    -- Functions
    --| "def" VariableId "::" Signature                                      { StFunctionDef ((Declaration $2 (snd $4) CatFunction) <$ $2) (fst $4) <$ $1 }
    --| "imp" VariableId "(" MaybeVariableList ")" "as" StatementList "end"  { StFunctionImp  $2 $4 $7 <$ $1 }
    | "def" VariableId ":" Signature Separator StatementList "end"      { undefined }
    --| VariableId "(" MaybeExpressionList ")"                               { StProcedureCall $1 $3 <$ $1 }
    --| "return" Expression                                                  { StReturn $2 <$ $1 }

    -- Conditional
    --| "if"     Expression "then" StatementList ElIfs "end"                      { StIf $2          $4 $5    <$ $1 }
    --| "unless" Expression "then" StatementList "end"                            { StIf (notExp $2) $4 empty <$ $1 }
    --| "unless" Expression "then" StatementList "else" StatementList "end"       { StIf (notExp $2) $4 $6    <$ $1 }
    --| "case" Expression MaybeNL WhenList "end"                                  { StCase $2 $4 empty <$ $1 }
    --| "case" Expression MaybeNL WhenList "otherwise" StatementList "end"        { StCase $2 $4 $6    <$ $1 }

    -- I/O
    --| "read" AccessList       { StRead  $2 <$ $1 }
    --| "print" ExpressionList    { StPrint $2 <$ $1 }

    -- Loops
    --| "while" Expression "do" StatementList "end"                                              { StLoop empty $2          $4    <$ $1 }
    --| "until" Expression "do" StatementList "end"                                              { StLoop empty (notExp $2) $4    <$ $1 }
    --| "repeat" StatementList "end" MaybeNL "while" Expression                                  { StLoop $2    $6          empty <$ $1 }
    --| "repeat" StatementList "end" MaybeNL "until" Expression                                  { StLoop $2    (notExp $6) empty <$ $1 }
    --| "repeat" StatementList "end" MaybeNL "while" Expression "do" StatementList "end"         { StLoop $2    $6          $8    <$ $1 }
    --| "repeat" StatementList "end" MaybeNL "until" Expression "do" StatementList "end"         { StLoop $2    (notExp $6) $8    <$ $1 }
    --| "for" VariableId "in" Expression "do" StatementList "end"                                { StFor $2 $4 $6 <$ $1 }
    | "break"           { StBreak    <$ $1 }
    | "continue"        { StContinue <$ $1 }

    -- Error
    --| error Separator   { Lex StNoop (0,0) }

VariableId :: { Lexeme Identifier }
    : varid         { unTkVarId `fmap` $1 }

TypeId :: { Lexeme Identifier }
    : typeid        { unTkTypeId `fmap` $1 }

FieldList :: { Seq (Lexeme Identifier, Lexeme DataType) }
    : Field                         { singleton $1 }
    | FieldList Separator Field     { $1 |> $5     }

Field :: { (Lexeme Identifier, Lexeme DataType) }
    : VariableList ":" DataType     { ($1, $3) }

VariableList :: { Seq (Lexeme Identifier) }
    : VariableId                    { singleton $1 }
    | VariableList "," VariableId   { $1     |> $3 }

Signature :: { (Seq (Lexeme DataType, Lexeme Identifier), Lexeme DataType) }
    : ParameterList "->" ReturnType                                 { ($1   , $3) }
    | "(" ParameterListWithNL ")" MaybeNL "->" MaybeNL ReturnType   { ($2   , $7) }
    | ReturnType                                                    { (empty, $1) }

ReturnType :: { Lexeme DataType }
    : DataType      { $1 }
    | "(" ")"       { Void <$ $1 }

ParameterList :: { Seq (Lexeme DataType, Lexeme Identifier) }
    : DataType VariableId                       { singleton ($1, $2) }
    | ParameterList "," DataType VariableId     { $1     |> ($3, $4) }

ParameterListWithNL :: { Seq (Lexeme DataType, Lexeme Identifier) }
    : DataType VariableId                                       { singleton ($1, $2) }
    | ParameterList MaybeNL "," MaybeNL DataType VariableId     { $1     |> ($3, $4) }
