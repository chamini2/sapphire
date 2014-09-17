{
{-# OPTIONS -w #-}
{-# LANGUAGE TupleSections #-}
module Parser (parseProgram) where

import           Lexer
import           Program
import           Position
import           Lexeme
import           Error         (Error)

--import           Control.Arrow (first)
--import           Data.Foldable as DF (concatMap, foldr)
import           Data.Maybe    (fromJust, isJust)
import           Data.Functor
import           Data.Sequence as DS hiding (length)
import           Prelude       hiding (concatMap, foldr)
}

%name parse
%tokentype { Lexeme Token }
%monad { Alex }
%lexer { lexWrap } { Lex TkEOF _ }
%error { parseError }

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

    -- Types
    --"Void"          { Lex TkVoidType    _ }
    --"Int"           { Lex TkIntType     _ }
    --"Float"         { Lex TkFloatType   _ }
    --"Bool"          { Lex TkBoolType    _ }
    --"Char"          { Lex TkCharType    _ }
    --"String"        { Lex TkStringType  _ }
    --"Range"         { Lex TkRangeType   _ }
    --"Type"          { Lex TkTypeType    _ }
    "record"        { Lex TkRecordType  _ }
    "union"         { Lex TkUnionType   _ }

    -- Statements
    -- -- Declarations
    "="             { Lex TkAssign      _ }
    "def"           { Lex TkDef         _ }
    "as"            { Lex TkAs          _ }
    ":"             { Lex TkSignature   _ }
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
    "=="            { Lex TkEqual       _ }
    "/="            { Lex TkUnequal     _ }
    "<"             { Lex TkLess        _ }
    ">"             { Lex TkGreat       _ }
    "<="            { Lex TkLessEq      _ }
    ">="            { Lex TkGreatEq     _ }
    "@"             { Lex TkBelongs     _ }

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
    : Statement                             { expandStatement $1       }
    | StatementList Separator Statement     { $1 >< expandStatement $3 }

Separator :: { () }
    : ";"           { }
    | newline       { }

MaybeNL :: { () }
    :                       { }
    | MaybeNL newline       { }

Statement :: { Lexeme Statement }
    : {- λ, no-op -}            { fillLex StNoop }
    | Access "=" Expression     { StAssign $1 $3 <$ $1 }

    -- Definitions
    | VariableList ":" DataType     { (StDeclarationList $ fmap (\idn -> Declaration idn $3 CatVariable <$ idn) $1) <$ index $1 0 }
    | "record" TypeId "as" FieldList "end"      { StStructDefinition (Record $2 $4 <$ $1) <$ $1 }
    | "union"  TypeId "as" FieldList "end"      { StStructDefinition (Union  $2 $4 <$ $1) <$ $1 }

    -- Functions
    | "def" VariableId ":" Signature Separator StatementList "end"      { StFunctionDef $2 $4 $6 <$ $1 }
    | VariableId "(" MaybeExpressionListNL ")"                          { StProcedureCall $1 $3 <$ $1 }
    | "return" Expression                                               { StReturn $2 <$ $1 }

    -- Conditionals
    | "if"     Expression MaybeNL "then" StatementList ElIfs "end"                  { StIf $2          $5 $6    <$ $1 }
    | "unless" Expression MaybeNL "then" StatementList "end"                        { StIf (notExp $2) $5 empty <$ $1 }
    | "unless" Expression MaybeNL "then" StatementList "else" StatementList "end"   { StIf (notExp $2) $5 $7    <$ $1 }
    | "case" Expression MaybeNL WhenList "end"                                      { StCase $2 $4 empty <$ $1 }
    | "case" Expression MaybeNL WhenList "otherwise" StatementList "end"            { StCase $2 $4 $6    <$ $1 }

    -- I/O
    | "read"              String         "," Access         { StReadString (Just $2) $4 <$ $1 }
    | "read"  "(" MaybeNL String MaybeNL "," AccessNL ")"   { StReadString (Just $4) $7 <$ $1 }
    | "read"      Access                                    { StReadString Nothing   $2 <$ $1 }
    | "read"  "(" AccessNL ")"                              { StReadString Nothing   $3 <$ $1 }
    | "print"     ExpressionList            { StPrintList $2 <$ $1 }
    | "print" "[" ExpressionListNL "]"      { StPrintList $3 <$ $1 }

    -- Loops
    | "while" Expression "do" StatementList "end"                                           { StLoop empty $2          $4    <$ $1 }
    | "until" Expression "do" StatementList "end"                                           { StLoop empty (notExp $2) $4    <$ $1 }
    | "repeat" StatementList "end" MaybeNL "while" Expression                               { StLoop $2    $6          empty <$ $1 }
    | "repeat" StatementList "end" MaybeNL "until" Expression                               { StLoop $2    (notExp $6) empty <$ $1 }
    | "repeat" StatementList "end" MaybeNL "while" Expression "do" StatementList "end"      { StLoop $2    $6          $8    <$ $1 }
    | "repeat" StatementList "end" MaybeNL "until" Expression "do" StatementList "end"      { StLoop $2    (notExp $6) $8    <$ $1 }
    | "for" VariableId "in" Expression "do" StatementList "end"                             { StFor  $2    $4          $6    <$ $1 }
    | "break"           { StBreak    <$ $1 }
    | "continue"        { StContinue <$ $1 }

    -- Error
    | error Separator   { fillLex StNoop }

---------------------------------------
-- Access

-- This parses 'array[0].field[1]' as '(((array)[0]).field)[1]'
Access :: { Lexeme Access }
    : VariableId                    { VariableAccess $1    <$ $1 }
    | Access "[" Expression "]"     { ArrayAccess    $1 $3 <$ $1 }
    -- This allows stuff like 'a . x = 0', accessing the field 'x' of variable 'a'
    | Access "." VariableId         { StructAccess   $1 $3 <$ $1 }

AccessNL :: { Lexeme Access }
    : MaybeNL VariableId MaybeNL                { VariableAccess $2    <$ $2 }
    | AccessNL "[" ExpressionNL "]" MaybeNL     { ArrayAccess    $1 $3 <$ $1 }
    | AccessNL "." MaybeNL VariableId MaybeNL   { StructAccess   $1 $4 <$ $1 }

---------------------------------------
-- Identifiers

VariableId :: { Lexeme Identifier }
    : varid         { unTkVarId `fmap` $1 }

TypeId :: { Lexeme Identifier }
    : typeid        { unTkTypeId `fmap` $1 }

VariableList :: { Seq (Lexeme Identifier) }
    : VariableId                    { singleton $1 }
    | VariableList "," VariableId   { $1     |> $3 }

---------------------------------------
-- Definitions

DataType :: { Lexeme DataType }
    : TypeId                 { DataType $1 <$ $1 }
    | DataType "[" Int "]"   { Array $1 $3 <$ $1 }

---------------------------------------
-- Structures

FieldList :: { Seq (Lexeme Identifier, Lexeme DataType) }
    : MaybeNL Field MaybeNL                 { expandField $2       }
    | FieldList "," MaybeNL Field MaybeNL   { $1 >< expandField $4 }

Field :: { (Seq (Lexeme Identifier), Lexeme DataType) }
    : VariableList ":" DataType     { ($1, $3) }

---------------------------------------
-- Function definition

Signature :: { Signature }
    : ParameterList "->" ReturnType                             { Sign $1    $3 }
    | "(" ParameterListNL ")" MaybeNL "->" MaybeNL ReturnType   { Sign $2    $7 }
    | ReturnType                                                { Sign empty $1 }

ReturnType :: { Lexeme DataType }
    : DataType      { $1 }
    | "(" ")"       { Void <$ $1 }

ParameterList :: { Seq (Lexeme Declaration) }
    : DataType VariableId                       { singleton (Declaration $2 $1 CatParameter <$ $1) }
    | ParameterList "," DataType VariableId     { $1     |> (Declaration $4 $3 CatParameter <$ $3) }

ParameterListNL :: { Seq (Lexeme Declaration) }
    : MaybeNL DataType VariableId MaybeNL                       { singleton (Declaration $3 $2 CatParameter <$ $2) }
    | ParameterListNL "," MaybeNL DataType VariableId MaybeNL   { $1     |> (Declaration $5 $4 CatParameter <$ $4) }

---------------------------------------
-- Conditionals

ElIfs :: { StBlock }
    :                                                      { empty }
    | "else" StatementList                                 { $2    }
    | "elif" Expression MaybeNL "then" StatementList ElIfs { singleton $ StIf $2 $5 $6 <$ $1 }

WhenList :: { Seq (Lexeme When) }
    : When              { singleton $1 }
    | WhenList When     { $1 |> $2     }

When :: { Lexeme When }
    : "when" ExpressionListNL "do" StatementList  { When $2 $4 <$ $1 }

---------------------------------------
-- Expressions

Int :: { Lexeme Int }
    : int           { unTkInt    `fmap` $1 }

Float :: { Lexeme Float }
    : float         { unTkFloat  `fmap` $1 }

Bool :: { Lexeme Bool }
    : "true"        { unTkBool   `fmap` $1 }
    | "false"       { unTkBool   `fmap` $1 }

Char :: { Lexeme Char }
    : char          { unTkChar   `fmap` $1 }

String :: { Lexeme String }
    : string        { unTkString `fmap` $1 }
    --: string        { LitString (unTkString `fmap` $1) (length . unTkString $ lexInfo $1) <$ $1 }

Expression :: { Lexeme Expression }
    -- Literals
    : Int           { LitInt    $1 <$ $1 }
    | Float         { LitFloat  $1 <$ $1 }
    | Bool          { LitBool   $1 <$ $1 }
    | Char          { LitChar   $1 <$ $1 }
    | String        { LitString $1 <$ $1 }
    -- Variable
    | Access                        { Variable $1 <$ $1 }
    -- Function call
    | VariableId "(" MaybeExpressionListNL ")"      { FunctionCall $1 $3 <$ $1 }
    -- Operators
    | Expression "+"   Expression   { ExpBinary (OpPlus    <$ $2) $1 $3 <$ $1 }
    | Expression "-"   Expression   { ExpBinary (OpMinus   <$ $2) $1 $3 <$ $1 }
    | Expression "*"   Expression   { ExpBinary (OpTimes   <$ $2) $1 $3 <$ $1 }
    | Expression "/"   Expression   { ExpBinary (OpDivide  <$ $2) $1 $3 <$ $1 }
    | Expression "%"   Expression   { ExpBinary (OpModulo  <$ $2) $1 $3 <$ $1 }
    | Expression "^"   Expression   { ExpBinary (OpPower   <$ $2) $1 $3 <$ $1 }
    | Expression ".."  Expression   { ExpBinary (OpFromTo  <$ $2) $1 $3 <$ $1 }
    | Expression "or"  Expression   { ExpBinary (OpOr      <$ $2) $1 $3 <$ $1 }
    | Expression "and" Expression   { ExpBinary (OpAnd     <$ $2) $1 $3 <$ $1 }
    | Expression "=="  Expression   { ExpBinary (OpEqual   <$ $2) $1 $3 <$ $1 }
    | Expression "/="  Expression   { ExpBinary (OpUnequal <$ $2) $1 $3 <$ $1 }
    | Expression "<"   Expression   { ExpBinary (OpLess    <$ $2) $1 $3 <$ $1 }
    | Expression "<="  Expression   { ExpBinary (OpLessEq  <$ $2) $1 $3 <$ $1 }
    | Expression ">"   Expression   { ExpBinary (OpGreat   <$ $2) $1 $3 <$ $1 }
    | Expression ">="  Expression   { ExpBinary (OpGreatEq <$ $2) $1 $3 <$ $1 }
    | Expression "@"   Expression   { ExpBinary (OpBelongs <$ $2) $1 $3 <$ $1 }
    | "-"   Expression              { ExpUnary  (OpNegate  <$ $1) $2    <$ $1 }
    | "not" Expression              { ExpUnary  (OpNot     <$ $1) $2    <$ $1 }
    | "(" ExpressionNL ")"          { lexInfo $2 <$ $1 }

ExpressionNL :: { Lexeme Expression }
    -- Literals
    : MaybeNL Int    MaybeNL    { LitInt    $2 <$ $2 }
    | MaybeNL Float  MaybeNL    { LitFloat  $2 <$ $2 }
    | MaybeNL Bool   MaybeNL    { LitBool   $2 <$ $2 }
    | MaybeNL Char   MaybeNL    { LitChar   $2 <$ $2 }
    | MaybeNL String MaybeNL    { LitString $2 <$ $2 }
    -- Variable
    | AccessNL                  { Variable $1 <$ $1 }
    -- Function call
    | MaybeNL VariableId "(" MaybeExpressionListNL ")" MaybeNL      { FunctionCall $2 $4 <$ $2 }
    --| MaybeNL string  MaybeNL           { LitString (unTkString `fmap` $2) (length . unTkString $ lexInfo $2) <$ $2 }
    -- Operators
    | ExpressionNL "+"   ExpressionNL   { ExpBinary (OpPlus    <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "-"   ExpressionNL   { ExpBinary (OpMinus   <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "*"   ExpressionNL   { ExpBinary (OpTimes   <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "/"   ExpressionNL   { ExpBinary (OpDivide  <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "%"   ExpressionNL   { ExpBinary (OpModulo  <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "^"   ExpressionNL   { ExpBinary (OpPower   <$ $2) $1 $3 <$ $1 }
    | ExpressionNL ".."  ExpressionNL   { ExpBinary (OpFromTo  <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "or"  ExpressionNL   { ExpBinary (OpOr      <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "and" ExpressionNL   { ExpBinary (OpAnd     <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "=="  ExpressionNL   { ExpBinary (OpEqual   <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "/="  ExpressionNL   { ExpBinary (OpUnequal <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "<"   ExpressionNL   { ExpBinary (OpLess    <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "<="  ExpressionNL   { ExpBinary (OpLessEq  <$ $2) $1 $3 <$ $1 }
    | ExpressionNL ">"   ExpressionNL   { ExpBinary (OpGreat   <$ $2) $1 $3 <$ $1 }
    | ExpressionNL ">="  ExpressionNL   { ExpBinary (OpGreatEq <$ $2) $1 $3 <$ $1 }
    | ExpressionNL "@"   ExpressionNL   { ExpBinary (OpBelongs <$ $2) $1 $3 <$ $1 }
    | "-"   ExpressionNL                { ExpUnary  (OpNegate  <$ $1) $2    <$ $1 }
    | "not" ExpressionNL                { ExpUnary  (OpNot     <$ $1) $2    <$ $1 }
    | "(" ExpressionNL ")"              { lexInfo $2 <$ $1 }

ExpressionList :: { Seq (Lexeme Expression) }
    : Expression                        { singleton $1 }
    | ExpressionList "," Expression     { $1 |> $3     }

ExpressionListNL :: { Seq (Lexeme Expression) }
    : MaybeNL Expression MaybeNL                        { singleton $2 }
    | ExpressionListNL "," MaybeNL Expression MaybeNL   { $1 |> $4     }

MaybeExpressionListNL :: { Seq (Lexeme Expression) }
    : MaybeNL               { empty }
    | ExpressionListNL      { $1    }


{

--------------------------------------------------------------------------------
-- Functions

expandStatement :: Lexeme Statement -> StBlock
expandStatement stL = case lexInfo stL of
    -- Removes no-op statements from statement blocks
    StNoop -> empty
    -- For the syntactic sugar of defining several variables of the same type using commas
    StDeclarationList dcls -> fmap (\dcl -> StVariableDeclaration dcl <$ dcl) dcls
    -- For the syntactic sugar of printing several strings with the same print statement using commas
    StPrintList exps -> fmap (\exp -> StPrint exp <$ stL) exps
    -- For the syntactic sugar of printing a string before reading a value
    StReadString mayStr accL -> if isJust mayStr
        then fromList [ StPrint (LitString (fromJust mayStr) <$ (fromJust mayStr)) <$ stL, StRead accL <$ accL ]
        else singleton $ StRead accL <$ accL
    -- No other statement needs compacting, yet
    _      -> singleton stL

-- For the syntactic sugar of defining several fields of the same type using commas
expandField :: (Seq (Lexeme Identifier), Lexeme DataType) -> Seq (Lexeme Identifier, Lexeme DataType)
expandField (idns, dt) = fmap (,dt) idns

notExp :: Lexeme Expression -> Lexeme Expression
notExp exp = (ExpUnary (OpNot <$ exp) exp) <$ exp

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
            -- Simulates that the String was correctly constructed
            cont $ TkString str <$ t
        _         -> cont t

parseError :: Lexeme Token -> Alex a
parseError (Lex t p) = fail $ show p ++ ": Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> (Seq Error, Program)
parseProgram input = runAlex' input parse

}
