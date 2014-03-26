{
{-# OPTIONS -w #-}
module Parser (parseProgram) where

import           Lexer
import           Language
import           Checker

import           Control.Arrow (first)
import           Data.Foldable as DF (concatMap, foldr)
import           Data.Functor
import           Data.Sequence hiding (reverse)
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
        "imp"           { Lex TkImp         _ }
        "as"            { Lex TkAs          _ }
        "::"            { Lex TkSignature   _ }
        "->"            { Lex TkArrow       _ }

        -- -- In/Out
        "read"          { Lex TkRead        _ }
        "print"         { Lex TkPrint       _ }

        -- -- Conditionals
        "if"            { Lex TkIf          _ }
        "then"          { Lex TkThen        _ }
        "elif"          { Lex TlElif        _ }
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

Statement :: { Lexeme Statement }
    :                               { Lex StNoop (0,0) }      -- λ, no-operation
    | VariableId "=" Expression     { StAssign $1 $3 <$ $1 }

    -- Definitions
    | "Record" TypeId "as" MaybeNL FieldList MaybeNL "end"      { makeStStructDeclaration (Record <$ $1) CatRecordField $2 (lexPosn $1) $5 }
    | "Union"  TypeId "as" MaybeNL FieldList MaybeNL "end"      { makeStStructDeclaration (Union  <$ $1)  CatUnionField  $2 (lexPosn $1) $5 }
    | DataType DeclareVariableList      { (StDeclarationList $ fmap (first (\iden -> Declaration iden $1 CatVariable <$ iden)) $2) <$ $1 }
    | "return" Expression               { StReturn $2 <$ $1 }

    -- Functions
    | "def" VariableId "::" Signature                                      { StFunctionDef ((Declaration $2 (snd $4) CatFunction) <$ $2) (fst $4) <$ $1 }
    | "imp" VariableId "(" MaybeVariableList ")" "as" StatementList "end"  { StFunctionImp  $2 $4 $7 <$ $1 }
    | VariableId "(" MaybeExpressionList ")"                               { StFunctionCall $1 $3    <$ $1 }

    -- Conditional
    | "if"     Expression "then" StatementList ElIfs "end"                      { StIf $2          $4 $5    <$ $1 }
    | "unless" Expression "then" StatementList "end"                            { StIf (notExp $2) $4 empty <$ $1 }
    | "unless" Expression "then" StatementList "else" StatementList "end"       { StIf (notExp $2) $4 $6    <$ $1 }
    | "case" Expression MaybeNL WhenList "end"                                  { StCase $2 $4 empty <$ $1 }
    | "case" Expression MaybeNL WhenList "otherwise" StatementList "end"        { StCase $2 $4 $6    <$ $1 }

    -- I/O
    | "read" VariableList       { StRead  $2 <$ $1 }
    | "print" ExpressionList    { StPrint $2 <$ $1 }

    -- Loops
    | "while" Expression "do" StatementList "end"                                              { StLoop empty $2          $4    <$ $1 }
    | "until" Expression "do" StatementList "end"                                              { StLoop empty (notExp $2) $4    <$ $1 }
    | "repeat" StatementList "end" MaybeNL "while" Expression                                  { StLoop $2    $6          empty <$ $1 }
    | "repeat" StatementList "end" MaybeNL "until" Expression                                  { StLoop $2    (notExp $6) empty <$ $1 }
    | "repeat" StatementList "end" MaybeNL "while" Expression "do" StatementList "end"         { StLoop $2    $6          $8    <$ $1 }
    | "repeat" StatementList "end" MaybeNL "until" Expression "do" StatementList "end"         { StLoop $2    (notExp $6) $8    <$ $1 }
    | "for" VariableId "in" Expression "do" StatementList "end"                                { StFor $2 $4 $6 <$ $1 }
    | "break"           { StBreak    <$ $1 }
    | "continue"        { StContinue <$ $1 }

    -- Error
    | error Separator   { Lex StNoop (0,0) }

VariableId :: { Lexeme Identifier }
    : varid         { unTkVarId `fmap` $1 }

TypeId :: { Lexeme Identifier }
    : typeid        { unTkTypeId `fmap` $1 }

FieldList :: { Seq (Lexeme (Category -> Declaration), Maybe (Lexeme Statement)) }
    : Field                                     { singleton $1 }
    | FieldList MaybeNL "," MaybeNL Field       { $1 |> $5     }

Field :: { (Lexeme (Category -> Declaration), Maybe (Lexeme Statement)) }
    : VariableId MaybeInitialized "::" DataType     { (Declaration $1 $4 <$ $1, (fmap (\expr -> StAssign $1 expr <$ $1) $2)) }

MaybeInitialized :: { Maybe (Lexeme Expression) }
    :                       { Nothing }
    | "=" Expression        { Just $2 }

MaybeNL :: { () }
    :                      { }
    | MaybeNL newline      { }

Separator :: { () }
    : ";"           { }
    | newline       { }

ElIfs :: { StBlock }
    :                           { empty }
    | "else" StatementList      { $2    }
    | "elif" Expression "then" StatementList ElIfs { singleton $ StIf $2 $4 $5 <$ $1 }

When :: { Lexeme When }
    : "when" ExpressionList "do" StatementList      { When $2 $4 <$ $1 }

WhenList :: { Seq (Lexeme When) }
    : When              { singleton $1 }
    | WhenList When     { $1 |> $2     }

---------------------------------------

DataType :: { Lexeme DataType }
    : "Int"             { Int     <$ $1 }
    | "Float"           { Float   <$ $1 }
    | "Bool"            { Bool    <$ $1 }
    | "Char"            { Char    <$ $1 }
    | "String"          { String  <$ $1 }
    | "Range"           { Range   <$ $1 }
    | "Type"            { Type    <$ $1 }
    | TypeId            { User $1 <$ $1 }
    --| "Union"  typeid   { Union  (unTkTypeId `fmap` $2) <$ $1 }
    --| "Record" typeid   { Record (unTkTypeId `fmap` $2) <$ $1 }
--    | "[" DataType "]"  { Array $2 <$ $1}
--    | "[" DataType "|" Expression "]"  { Array $2 $3 <$ $1}

VariableList :: { Seq (Lexeme Identifier) }
    : VariableId                         { singleton $1  }
    | VariableList "," VariableId        { $1 |> $3      }

MaybeVariableList :: { Seq (Lexeme Identifier) }
    :                           { empty }
    | VariableList              { $1    }

DeclareVariableList :: { Seq (Lexeme Identifier, Maybe (Lexeme Expression)) }
    : VariableId MaybeInitialized                               { singleton ($1, $2) }
    | DeclareVariableList "," VariableId MaybeInitialized       { $1     |> ($3, $4) }

DataTypeList :: { Seq (Lexeme DataType) }
    : DataType                   { singleton $1 }
    | DataTypeList "," DataType  { $1 |> $3     }

MaybeDataTypeList :: { Seq (Lexeme DataType) }
    :                   { empty }
    | DataTypeList      { $1    }

Signature :: { (Seq (Lexeme DataType), Lexeme DataType) }
    : "(" MaybeDataTypeList ")" SignatureReturn { ($2,$4) }
    | DataTypeList              SignatureReturn { ($1,$2) }

SignatureReturn :: { Lexeme DataType }
    : "->" DataType         { $2 }
    |                       { Lex Void (0,0) }

---------------------------------------

Expression :: { Lexeme Expression }
    -- Variable
    : VariableId                                 { Variable $1 <$ $1 }
    -- Function call
    | VariableId "(" MaybeExpressionList ")"     { FunctionCall $1 $3 <$ $1 }
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

MaybeExpressionList :: { Seq (Lexeme Expression) }
    :                       { empty }
    | ExpressionList        { $1    }

{

--------------------------------------------------------------------------------
-- Functions

notExp :: Lexeme Expression -> Lexeme Expression
notExp exp = (ExpUnary (OpNot <$ exp) exp) <$ exp

makeStStructDeclaration :: Lexeme DataType -> Category -> Lexeme Identifier -> Position ->
    Seq (Lexeme (Category -> Declaration) , Maybe (Lexeme Statement)) -> Lexeme Statement
makeStStructDeclaration dt cat iden p ls = Lex (StStructDeclaration decl list) p
    where
        list = fmap (uncurry func) ls
        decl = Lex (Declaration iden dt CatDataType) p
        func (Lex toDcl posn) maySt = (Lex (toDcl cat) posn, maySt)

filterSt :: Lexeme Statement -> StBlock
filterSt st = case lexInfo st of
    StNoop                 -> empty
    StDeclarationList dcls -> fromList $ concatMap (uncurry func) dcls
        where
            func dcl@(Lex (Declaration var _ _) _) mayExpr = (StDeclaration dcl <$ dcl) : case mayExpr of
                Just expr -> [StAssign var expr <$ expr]
                Nothing   -> []
    -- No other statement needs a filter
    _ -> singleton st


--parseError :: Lexeme Token -> Checker ()
--parseError (Lex tk posn) = do
--    tell (singleton $ PError posn $ UnexpectedToken (show tk))

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
happyError (Lex t p) = fail $ showPosn p ++ ": Parse error on Token: " ++ show t ++ "\n"

parseProgram :: String -> (Seq CheckError, Program)
parseProgram input = runAlex' input parse

}
