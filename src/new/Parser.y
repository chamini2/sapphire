{
{-# OPTIONS -w #-}
{-# LANGUAGE TupleSections #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Error         (Error)

--import           Control.Arrow (first)
--import           Data.Foldable as DF (concatMap, foldr)
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
    : {- λ, no-op -}            { Lex StNoop (Posn (0,0)) }
    --| Access "=" Expression     { StAssign $1 $3 <$ $1 }

    -- Definitions
    | "Record" TypeId "as" FieldList "end"      { StStructDefinition (Record $2 $4 (-1) <$ $1) <$ $1 }
    | "Union"  TypeId "as" FieldList "end"      { StStructDefinition (Union  $2 $4 (-1) <$ $1) <$ $1 }
    | VariableList ":" DataType     { (StDeclarationList $ fmap (\iden -> Declaration iden $3 CatVariable <$ iden) $1) <$ $3 }

    -- Functions
    --| "def" VariableId "::" Signature                                      { StFunctionDef ((Declaration $2 (snd $4) CatFunction) <$ $2) (fst $4) <$ $1 }
    --| "imp" VariableId "(" MaybeVariableList ")" "as" StatementList "end"  { StFunctionImp  $2 $4 $7 <$ $1 }
    | "def" VariableId ":" Signature Separator StatementList "end"      { StNoop <$ $1 }
    --| VariableId "(" MaybeExpressionList ")"                               { StProcedureCall $1 $3 <$ $1 }
    | "return" Expression                                               { StReturn $2 <$ $1 }

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
    | error Separator   { Lex StNoop (Posn (0,0)) }

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
-- Structures

FieldList :: { Seq (Lexeme Identifier, Lexeme DataType) }
    : MaybeNL Field MaybeNL                 { expandField $2       }
    | FieldList "," MaybeNL Field MaybeNL   { $1 >< expandField $4 }

Field :: { (Seq (Lexeme Identifier), Lexeme DataType) }
    : VariableList ":" DataType     { ($1, $3) }

---------------------------------------
-- Function definition

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
    : MaybeNL DataType VariableId MaybeNL                           { singleton ($2, $3) }
    | ParameterListWithNL "," MaybeNL DataType VariableId MaybeNL   { $1     |> ($4, $5) }

---------------------------------------

DataType :: { Lexeme DataType }
    : "Int"                         { Int           <$ $1 }
    | "Float"                       { Float         <$ $1 }
    | "Bool"                        { Bool          <$ $1 }
    | "Char"                        { Char          <$ $1 }
    | "String"                      { String (-1)   <$ $1 }
    | "Range"                       { Range         <$ $1 }
    | "Type"                        { Type          <$ $1 }
    --| TypeId                        { UserDef $1    <$ $1 }
    --| DataType "[" Expression "]"   { Array $1 $3 0 <$ $1 }

---------------------------------------

Expression :: { Lexeme Expression }
    -- Variable
    --: Access                        { Variable $1 <$ $1 }
    -- Function call
    --| VariableId "(" MaybeExpressionList ")"     { FunctionCall $1 $3 <$ $1 }
    -- Literals
    --| int                           { LitInt    (unTkInt    `fmap` $1)                                    <$ $1 }
    : int                           { LitInt    (unTkInt    `fmap` $1)                                    <$ $1 }
    | float                         { LitFloat  (unTkFloat  `fmap` $1)                                    <$ $1 }
    | char                          { LitChar   (unTkChar   `fmap` $1)                                    <$ $1 }
    | string                        { LitString (unTkString `fmap` $1) (length . unTkString $ lexInfo $1) <$ $1 }
    | "true"                        { LitBool   (unTkBool   `fmap` $1)                                    <$ $1 }
    | "false"                       { LitBool   (unTkBool   `fmap` $1)                                    <$ $1 }
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
    | "(" Expression ")"            { lexInfo $2 <$ $1 }

ExpressionList :: { Seq (Lexeme Expression) }
    : Expression                        { singleton $1 }
    | ExpressionList "," Expression     { $1 |> $3     }

MaybeExpressionList :: { Seq (Lexeme Expression) }
    :                       { empty }
    | ExpressionList        { $1    }


{

--------------------------------------------------------------------------------
-- Functions

expandStatement :: Lexeme Statement -> StBlock
expandStatement st = case lexInfo st of
    -- Removes no-op statements from statement blocks
    StNoop -> empty
    -- For the syntactic sugar of defining several variables of the same type using commas
    StDeclarationList dcls -> fmap (\dcl -> StDeclaration dcl <$ dcl) dcls
    -- No other statement needs compacting, yet
    _      -> singleton st

-- For the syntactic sugar of defining several fields of the same type using commas
expandField :: (Seq (Lexeme Identifier), Lexeme DataType) -> Seq (Lexeme Identifier, Lexeme DataType)
expandField (idens, dt) = fmap (,dt) idens

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

happyError :: Lexeme Token -> Alex a
happyError (Lex t p) = fail $ show p ++ ": Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> (Seq Error, Program)
parseProgram input = runAlex' input parse

}
