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
    | Variable_ ":=" Expression_ { SappStmtAssignment $1 $3 }
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
    | "+" Expression_ Expression_ { SappExpAddition $2 $3 }
    | "-" Expression_ Expression_ { SappExpSubstraction $2 $3 }
    | "*" Expression_ Expression_ { SappExpMultiplication $2 $3 }
    | "/" Expression_ Expression_ { SappExpDivision $2 $3 }
    | "%" Expression_ Expression_ { SappExpModulo $2 $3 }
    | "^" Expression_ Expression_ { SappExpExponentiation $2 $3 }
    | "or" Expression_ Expression_ { SappExpConjuction $2 $3 }
    | "and" Expression_ Expression_ { SappExpDisjunction $2 $3 }
    | "not" Expression_ { SappExpNegation $2 }
    | "=" Expression_ Expression_ { SappExpEqualsTo $2 $3 }
    | "/=" Expression_ Expression_ { SappExpDifferentFrom $2 $3 }
    | ">" Expression_ Expression_ { SappExpGreaterThan $2 $3 }
    | ">=" Expression_ Expression_ { SappExpGreaterThanOrEqualTo $2 $3 }
    | "<" Expression_ Expression_ { SappExpLessThan $2 $3 }
    | "<=" Expression_ Expression_ { SappExpLessThanOrEqualto $2 $3 }

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
    | SappExpAddition SappExpression SappExpression
    | SappExpSubstraction SappExpression SappExpression
    | SappExpMultiplication SappExpression SappExpression
    | SappExpDivision SappExpression SappExpression
    | SappExpModulo SappExpression SappExpression
    | SappExpExponentiation SappExpression SappExpression
    | SappExpConjuction SappExpression SappExpression
    | SappExpDisjunction SappExpression SappExpression
    | SappExpNegation SappExpression
    | SappExpEqualsTo SappExpression SappExpression
    | SappExpDifferentFrom SappExpression SappExpression
    | SappExpGreaterThan SappExpression SappExpression
    | SappExpGreaterThanOrEqualTo SappExpression SappExpression
    | SappExpLessThan SappExpression SappExpression
    | SappExpLessThanOrEqualto SappExpression SappExpression
    deriving (Show, Eq)

data SappVariable = SappVar String
    deriving (Show, Eq)

}
