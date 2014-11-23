{
{-# OPTIONS_GHC -w #-}
module Language.Sapphire.Parser
    ( parseProgram
    ) where

import           Language.Sapphire.Error
import           Language.Sapphire.Lexer
import           Language.Sapphire.Program

import           Control.Monad             (unless)
import           Data.Foldable             (toList)
import           Data.Functor              ((<$), (<$>))
import           Data.Maybe                (fromJust, isJust)
import           Data.Sequence             (Seq, empty, fromList, index,
                                            singleton, (><), (|>), (<|))
}

%name parse
%tokentype { Lexeme Token }
%monad { Alex }
%lexer { lexWrap } { Lex TkEOF _ }
%error { parseError }

%token

    -- Language
    newline         { Lex TkNewLine     _ }
    "main"          { Lex TkMain        _ }
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
    "this"          { Lex TkThis        _ }
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

    -- -- String
    "++"            { Lex TkConcat      _ }

    -- -- Identifiers
    ident           { Lex (TkIden   _)  _ }
    typeid          { Lex (TkTypeId _)  _ }

--------------------------------------------------------------------------------

-- Precedence
-- -- Bool
%left     "or"
%left     "and"
%right    "not"

-- -- String
%right    "++"

-- -- -- Compare
%nonassoc "@"
%left     "==" "/="
%nonassoc "<" "<=" ">" ">="

-- -- Arithmetic
%left     ".."
%left     "+" "-"
%left     "*" "/" "%"
%right    "-"
%right    "^"

%%

--------------------------------------------------------------------------------
-- Grammar

Program :: { Program }
    : TopStatementList      { Program $1 }

Separator :: { () }
    : ";"           { }
    | newline       { }

MaybeNL :: { () }
    : {- λ -}               { }
    | MaybeNL newline       { }

TopStatementList :: { StBlock }
    : TopStatement                                  { expandStatement $1       }
    | BothStatement                                 { expandStatement $1       }
    | TopStatementList Separator TopStatement       { $1 >< expandStatement $3 }
    | TopStatementList Separator BothStatement      { $1 >< expandStatement $3 }

TopStatement :: { Lexeme Statement }
    -- Definitions
    : Structure TypeId "as" FieldList "end"     { StStructDefinition ($1 <*> pure $2) $4 <$ $1 }

    -- Functions
    | "def" VariableId ":" Signature Separator StatementList "end"      { StFunctionDef $2 $4 $6 <$ $1 }

    -- Main
    | "main" StatementList "end"    { StFunctionDef (mainName <$ $1) (Sign empty (pure $ DataType $ pure "Int")) (addReturn $2) <$ $1 }

    -- Errors
    -- -- Definitions
    | Structure        "as" FieldList "end"     {% tellPError (lexPosn $2) TypeDefinitionIdentifier >> return (pure StNoop) }
    | Structure TypeId "as"           "end"     {% tellPError (lexPosn $2) NoFieldsInType           >> return (pure StNoop) }
    | Structure        "as"           "end"     {% tellPError (lexPosn $2) TypeDefinitionIdentifier >>
                                                   tellPError (lexPosn $2) NoFieldsInType           >> return (pure StNoop) }
    -- -- Functions
    | "def"            ":" Signature Separator StatementList "end"      {% tellPError (lexPosn $2) FunctionDefinitionIdentifier >> return (pure StNoop) }
    -- -- Inner statements
    | InnerStatement        {% tellPError (lexPosn $1) InnerStatementAsTopStatement >> return (pure StNoop) }

----------------------------------------

StatementList :: { StBlock }
    : InnerStatement                            { expandStatement $1       }
    | BothStatement                             { expandStatement $1       }
    | StatementList Separator InnerStatement    { $1 >< expandStatement $3 }
    | StatementList Separator BothStatement     { $1 >< expandStatement $3 }

InnerStatement :: { Lexeme Statement }
    -- Assignment
    : Access "=" Expression     { StAssign $1 $3 <$ $1 }

    -- Functions
    | VariableId "(" MaybeExpressionListNL ")"                          { StProcedureCall $1 $3 <$ $1 }
    | "return" Expression                                               { StReturn (Just $2) <$ $1 }
    | "return"                                                          { StReturn Nothing   <$ $1 }

    -- Conditionals
    | "if"     Expression MaybeNL "then" StatementList ElIfList "end"               { StIf $2          $5 $6    <$ $1 }
    | "unless" Expression MaybeNL "then" StatementList "end"                        { StIf (notExp $2) $5 empty <$ $1 }
    | "unless" Expression MaybeNL "then" StatementList "else" StatementList "end"   { StIf (notExp $2) $5 $7    <$ $1 }
    | "case" Expression MaybeNL WhenList "end"                                      { StCase $2 $4 empty <$ $1 }
    | "case" Expression MaybeNL WhenList "otherwise" StatementList "end"            { StCase $2 $4 $6    <$ $1 }

    -- I/O
    | "read"              String         "," AccessList         { StReadString (Just $2) $4 <$ $1 }
    | "read"  "(" MaybeNL String MaybeNL "," AccessListNL ")"   { StReadString (Just $4) $7 <$ $1 }
    | "read"      AccessList                                    { StReadString Nothing   $2 <$ $1 }
    | "read"  "(" AccessListNL ")"                              { StReadString Nothing   $3 <$ $1 }
    | "print"     ExpressionList            { StPrintList $2 <$ $1 }
    | "print" "[" ExpressionListNL "]"      { StPrintList $3 <$ $1 }

    -- Loops
    |                                       "while" Expression "do" StatementList "end"     { StLoop empty $2 $4    <$ $1 }
    | "repeat" StatementList "this" MaybeNL "while" Expression                              { StLoop $2    $6 empty <$ $1 }
    | "repeat" StatementList "this" MaybeNL "while" Expression "do" StatementList "end"     { StLoop $2    $6 $8    <$ $1 }
    |                                       "until" Expression "do" StatementList "end"     { StLoop empty (notExp $2) $4    <$ $1 }
    | "repeat" StatementList "this" MaybeNL "until" Expression                              { StLoop $2    (notExp $6) empty <$ $1 }
    | "repeat" StatementList "this" MaybeNL "until" Expression "do" StatementList "end"     { StLoop $2    (notExp $6) $8    <$ $1 }

    | "for" VariableId "in" Expression "do" StatementList "end"                             { StFor $2 $4 $6 <$ $1 }
    | "break"           { StBreak    <$ $1 }
    | "continue"        { StContinue <$ $1 }

    -- Errors
    -- -- Assignment
    | Access "="                {% tellPError (lexPosn $2) AssignmentMissingExpression >> return (pure StNoop) }
    |        "=" Expression     {% tellPError (lexPosn $1) AssignmentMissingAccess     >> return (pure StNoop) }

    -- -- Conditionals
    | "case" Expression MaybeNL          "end"                                      {% tellPError (lexPosn $4) NoWhensInCase >> return (pure StNoop) }
    | "case" Expression MaybeNL          "otherwise" StatementList "end"            {% tellPError (lexPosn $4) NoWhensInCase >> return (pure StNoop) }

---------------------------------------
-- Statements in TopStatement and InnerStatement

BothStatement :: { Lexeme Statement }
    : {- λ, no-op -}                { pure StNoop }
    -- Definitions
    | VariableList ":" DataType     { (StDeclarationList $ fmap (\idn -> Declaration idn $3 CatVariable <$ idn) $1) <$ index $1 0 }
    -- Errors
    | VariableList ":"              {% tellPError (lexPosn $2) VariableDefinitionWithoutDataType >> return (pure StNoop) }

---------------------------------------
-- Structures

Structure :: { Lexeme (Lexeme Identifier -> DataType) }
    : "record"      { Record <$ $1}
    | "union"       { Union  <$ $1}

---------------------------------------
-- Access

-- This parses 'array[0].field[1]' as '(((array)[0]).field)[1]'
Access :: { Lexeme Access }
    : VariableId                    { VariableAccess $1    <$ $1 }
    -- This allows stuff like 'a [ 2 ] = 0', accessing the index '2' of array 'a'
    | Access "[" Expression "]"     { ArrayAccess    $1 $3 <$ $1 }
    -- This allows stuff like 'a . x = 0', accessing the field 'x' of variable 'a'
    | Access "." VariableId         { StructAccess   $1 $3 <$ $1 }

AccessNL :: { Lexeme Access }
    : MaybeNL VariableId MaybeNL                { VariableAccess $2    <$ $2 }
    | AccessNL "[" ExpressionNL "]" MaybeNL     { ArrayAccess    $1 $3 <$ $1 }
    | AccessNL "." MaybeNL VariableId MaybeNL   { StructAccess   $1 $4 <$ $1 }

AccessList :: { Seq (Lexeme Access) }
    : Access                    { singleton $1 }
    | AccessList "," Access     { $1     |> $3 }

AccessListNL :: { Seq (Lexeme Access) }
    : AccessNL                      { singleton $1 }
    | AccessListNL "," AccessNL     { $1     |> $3 }

---------------------------------------
-- Identifiers

VariableId :: { Lexeme Identifier }
    : ident         { unTkIden `fmap` $1 }
    -- Errors
    | typeid        {% do
                        let const = unTkTypeId `fmap` $1
                        tellPError (lexPosn $1) (TypeIdInsteadOfVarId $ lexInfo const)
                        return const
                    }

TypeId :: { Lexeme Identifier }
    : typeid        { unTkTypeId `fmap` $1 }
    -- Errors
    | ident         {% do
                        let const = unTkIden `fmap` $1
                        tellPError (lexPosn $1) (VarIdInsteadOfTypeId $ lexInfo const)
                        return const
                    }

VariableList :: { Seq (Lexeme Identifier) }
    : VariableId                    { singleton $1 }
    | VariableList "," VariableId   { $1     |> $3 }
    -- Errors
    | VariableList     VariableId   {% tellPError (lexPosn $2) VariableListComma >> return ($1 |> $2) }

---------------------------------------
-- Definitions

BasicDataType :: { Lexeme DataType }
    : TypeId    { DataType $1 <$ $1 }

DataType :: { Lexeme DataType }
    : BasicDataType             { $1 }
    | "[" Int "]" DataType      {% unless (lexInfo $2 > 0) (tellPError (lexPosn $2) ArraySize) >> return (Array $4 $2 <$ $1) }
    -- Errors
    |  "[" "-" Int "]" DataType {% tellPError (lexPosn $2) ArraySize >> return (Array $5 (negate `fmap` $3) <$ $1) }
    | "["     "]" DataType      {% tellPError (lexPosn $2) ArrayDataTypeSize >> return $3 }
    |     Int     DataType      {% tellPError (lexPosn $2) ArrayDataTypeSize >> return (Array $2 $1 <$ $1) }
    | "[" Access "]" DataType   {% tellPError (lexPosn $2) ArrayDataTypeSize >> return $4 }

---------------------------------------
-- Structures

FieldList :: { Seq Field }
    : MaybeNL Field MaybeNL                 { $2       }
    | FieldList "," MaybeNL Field MaybeNL   { $1 >< $4 }
    -- Errors
    | FieldList             Field MaybeNL   {% tellPError (lexPosn . snd $ index $2 0) FieldListComma >> return ($1 >< $2) }

Field :: { Seq Field }
    : VariableList ":" DataType     { expandField $3 $1 }

---------------------------------------
-- Function definition

Signature :: { Signature }
    : ParameterList "->" ReturnType                             { Sign $1    $3 }
    | "(" ParameterListNL ")" MaybeNL "->" MaybeNL ReturnType   { Sign $2    $7 }
    | ReturnType                                                { Sign empty $1 }

ReturnType :: { Lexeme DataType }
    : BasicDataType         { $1 }
    | "(" ")"               { Void <$ $1 }
    -- Errors
    | "[" "]" SignType      {% tellPError (lexPosn $1) ArrayReturn >> return $3 }

ParameterList :: { Seq (Lexeme Declaration) }
    : Parameter                       { singleton $1 }
    | ParameterList "," Parameter     { $1     |> $3 }
    -- Errors
    | ParameterList     Parameter     {% tellPError (lexPosn $2) ParameterListComma >> return ($1 |> $2) }

ParameterListNL :: { Seq (Lexeme Declaration) }
    : MaybeNL Parameter MaybeNL                       { singleton $2 }
    | ParameterListNL "," MaybeNL Parameter MaybeNL   { $1     |> $4 }
    -- Errors
    | ParameterListNL             Parameter MaybeNL   {% tellPError (lexPosn $2) ParameterListComma >> return ($1 |> $2) }

SignType :: { Lexeme DataType }
    : BasicDataType         { $1 }
    | "[" "]" SignType      { ArraySign $3 <$ $1 }

Parameter :: { Lexeme Declaration }
    : SignType VariableId   { Declaration $2 $1 CatParameter <$ $1 }

---------------------------------------
-- Conditionals

ElIfList :: { StBlock }
    : {- λ -}                                                   { empty }
    | "else" StatementList                                      { $2    }
    | "elif" Expression MaybeNL "then" StatementList ElIfList   { singleton $ StIf $2 $5 $6 <$ $1 }

WhenList :: { Seq (Lexeme When) }
    : When              { singleton $1 }
    | WhenList When     { $1     |> $2 }

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
    | Expression "++"  Expression   {% tellLError (lexPosn $2) (TokenNotSupported TkConcat) >> return $1 }
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
    | ExpressionNL "++"  ExpressionNL   {% tellLError (lexPosn $2) (TokenNotSupported TkConcat) >> return $1 }
    | "-"   ExpressionNL                { ExpUnary  (OpNegate  <$ $1) $2    <$ $1 }
    | "not" ExpressionNL                { ExpUnary  (OpNot     <$ $1) $2    <$ $1 }
    | "(" ExpressionNL ")"              { lexInfo $2 <$ $1 }

ExpressionList :: { Seq (Lexeme Expression) }
    : Expression                        { singleton $1 }
    | ExpressionList "," Expression     { $1     |> $3 }

ExpressionListNL :: { Seq (Lexeme Expression) }
    : MaybeNL Expression MaybeNL                        { singleton $2 }
    | ExpressionListNL "," MaybeNL Expression MaybeNL   { $1     |> $4 }

MaybeExpressionListNL :: { Seq (Lexeme Expression) }
    : MaybeNL               { empty }
    | ExpressionListNL      { $1    }

{

--------------------------------------------------------------------------------
-- Functions

addReturn :: StBlock -> StBlock
addReturn = flip (|>) (pure . StReturn . Just . pure . LitInt $ pure 0)

expandStatement :: Lexeme Statement -> StBlock
expandStatement stL = case lexInfo stL of
    -- Removes no-op statements from statement blocks
    StNoop -> empty
    -- For the syntactic sugar of defining several variables of the same type using commas
    StDeclarationList dcls -> fmap (\dcl -> StVariableDeclaration dcl <$ dcl) dcls
    -- For the syntactic sugar of printing several strings with the same print statement using commas
    StPrintList exps -> fmap (\exp -> StPrint exp <$ stL) exps
    -- For the syntactic sugar of printing a string before reading values
    -- For the syntactic sugar of reading several values with the same read statement using commas
    StReadString mayStr accLs -> maybe id (\strL -> (<|) (StPrint (LitString strL <$ strL) <$ strL)) mayStr $ fmap (\accL -> StRead accL <$ accL) accLs
    -- No other statement needs expanding
    _ -> singleton stL

-- For the syntactic sugar of defining several fields of the same type using commas
expandField :: Lexeme DataType -> Seq (Lexeme Identifier) -> Seq Field
expandField dtL idnLs = fromList $ zip (toList idnLs) (repeat dtL)

notExp :: Lexeme Expression -> Lexeme Expression
notExp exp = (ExpUnary (OpNot <$ exp) exp) <$ exp

--------------------------------------------------------------------------------
-- Parser

lexWrap :: (Lexeme Token -> Alex a) -> Alex a
lexWrap cont = do
    t <- alexMonadScan
    case t of
        Lex (TkError c) p -> do
            tellLError p (UnexpectedChar c)
            lexWrap cont
        Lex (TkStringError str) p -> do
            tellLError p (StringError str)
            -- Simulates that the String was correctly constructed
            cont $ TkString str <$ t
        -- Any other Token is part of the language
        _ -> cont t

parseError :: Lexeme Token -> Alex a
parseError (Lex t p) = fail $ show p ++ ": Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> (Program, Seq Error)
parseProgram input = runAlex' input parse

}
