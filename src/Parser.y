{
module Parser
    ( parseProgram
    ) where

import Lexer
}

%name parseProgram Program_
%tokentype { Token }
%error { parseError }

%token

    "main" { TkMain _ }
    "end" { TkEnd _ }
    "begin" { TkBegin _ }
    "read" { TkRead _ }
    "write" { TkWrite _ }
    "if" { TkIf _ }
    "then" { TkThen _ }
    "else" { TkElse _ }
    "integer" { TkInteger _ }
    "boolean" { TkBoolean _ }
    ";" { TkSemicolon _ }
    ":=" { TkAssign _ }
    "+" { TkAddition _ }
    "-" { TkSubtraction _ }
    "*" { TkMultiplication _ }
    "/" { TkDivision _ }
    "%" { TkModulo _ }
    "^" { TkExponentiation _ }
    "or" { TkDisjunction _ }
    "and" { TkConjuction _ }
    "not" { TkNegation _ }
    "=" { TkEqualsTo _ }
    "/=" { TkDifferentFrom _ }
    ">" { TkGreaterThan _ }
    ">=" { TkGreaterThanOrEquals _ }
    "<" { TkLessThan _ }
    "<=" { TkLessThanOrEquals _ }
    charstring_ { TkLitCharString $$ _ }
    integer_ { TkLitInteger $$ _ }
    boolean_ { TkLitBoolean $$ _ }
    identifier_ { TkIdentifier $$ _ }
%%

Program_
    : "main" StatementList_ "end" { ASTStmtBlock $2 }
    | StatementList_ { ASTStmtBlock $1 }

StatementList_
    : {- empty -} { mempty }
    | Statement_ ";" StatementList_ { $1 : $3 }

Statement_
    : "begin" StatementList_ "end" { ASTStmtBlock $2 }

{
parseError = undefined

data ASTStatement
    = ASTStmtBlock [ASTStatement]
    | ASTStmtVariableDeclaration String String
    deriving (Show)
}
