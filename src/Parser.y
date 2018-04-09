{
module Parser
    ( parseProgram
    , parseStatement
    , parseExpression
    , SappStatement(..)
    , SappDataType(..)
    , SappExpression(..)
    , SappVariable(..)
    ) where

import Lexer
import PrettyShow
}

%name parseProgram Program_
%name parseStatement Statement_
%name parseExpression Expression_
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
    "," { TkComma _ }
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

-- general parsers
SND(fst, snd)
    : fst snd { $2 }

MAYBE(val)
    : {- empty -} { Nothing }
    | val { Just $1 }

EITHER(lft,rgt)
    : lft { Left $1 }
    | rgt { Right $1 }

LIST_END_SEP0(elm,sep)
    : {- empty -} { mempty }
    | elm sep LIST_END_SEP0(elm,sep) { pure $1 `mappend` $3 }

LIST_SEP1(elm,sep)
    : elm { pure $1 }
    | LIST_SEP1(elm,sep) sep elm { $1 `mappend` pure $3 }

--------------------------------------------------------------------------------

Program_
    : "main" StatementList_ "end" { SappStmtBlock $2 }

StatementList_
    : LIST_END_SEP0(Statement_, ";") { $1 }

Statement_
    : "begin" StatementList_ "end" { SappStmtBlock $2 }
    | DataType_ identifier_ { SappStmtVariableDeclaration $1 $2 }
    | "read" Variable_ { SappStmtRead $2 }
    | "write" LIST_SEP1(EITHER(charstring_, Expression_), ",") { SappStmtWrite $2 }
    | "if" Expression_ "then" Statement_ MAYBE(SND("else", Statement_)) { SappStmtIf $2 $4 $5 }

DataType_
    : "integer" { SappDTInteger }
    | "boolean" { SappDTBoolean }

Expression_
    : integer_ { SappExpLitInteger $1 }
    | boolean_ { SappExpLitBoolean $1 }
    | Variable_ { SappExpVariable $1 }

Variable_
    : identifier_ { SappVar $1 }
{
parseError :: [Token] -> a
parseError (tok:_) = error $ (prettyShow $ posn tok) ++ ": " ++ (prettyShow tok)

data SappStatement
    = SappStmtBlock [SappStatement]
    | SappStmtVariableDeclaration SappDataType String
    | SappStmtAssignment SappVariable SappExpression
    | SappStmtRead SappVariable
    | SappStmtWrite [Either String SappExpression]
    | SappStmtIf SappExpression SappStatement (Maybe SappStatement)
    deriving (Show, Eq)

data SappDataType
    = SappDTInteger
    | SappDTBoolean
    deriving (Show, Eq)

data SappExpression
    = SappExpLitInteger Integer
    | SappExpLitBoolean Bool
    | SappExpVariable SappVariable
    deriving (Show, Eq)

data SappVariable = SappVar String
    deriving (Show, Eq)

}
