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
    "(" { TkParenthesisL _ }
    ")" { TkParenthesisR _ }
    "+" { TkAddition _ }
    "-" { TkSubtraction _ }
    "*" { TkMultiplication _ }
    "/" { TkDivision _ }
    "%" { TkModulo _ }
    "^" { TkExponentiation _ }
    "~" { TkIntNegation _ }
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
    | "(" Expression_ ")" { $2 }
    | "+" Expression_ Expression_ { evaluateToLitExp $ SappExpAddition $2 $3 }
    | "-" Expression_ Expression_ { evaluateToLitExp $ SappExpSubtraction $2 $3 }
    | "*" Expression_ Expression_ { evaluateToLitExp $ SappExpMultiplication $2 $3 }
    | "/" Expression_ Expression_ { evaluateToLitExp $ SappExpDivision $2 $3 }
    | "%" Expression_ Expression_ { evaluateToLitExp $ SappExpModulo $2 $3 }
    | "^" Expression_ Expression_ { evaluateToLitExp $ SappExpExponentiation $2 $3 }
    | "~" Expression_ { evaluateToLitExp $ SappExpIntNegation $2 }
    | "or" Expression_ Expression_ { evaluateToLitExp $ SappExpConjuction $2 $3 }
    | "and" Expression_ Expression_ { evaluateToLitExp $ SappExpDisjunction $2 $3 }
    | "not" Expression_ { evaluateToLitExp $ SappExpNegation $2 }
    | "=" Expression_ Expression_ { evaluateToLitExp $ SappExpEqualsTo $2 $3 }
    | "/=" Expression_ Expression_ { evaluateToLitExp $ SappExpDifferentFrom $2 $3 }
    | ">" Expression_ Expression_ { evaluateToLitExp $ SappExpGreaterThan $2 $3 }
    | ">=" Expression_ Expression_ { evaluateToLitExp $ SappExpGreaterThanOrEqualsTo $2 $3 }
    | "<" Expression_ Expression_ { evaluateToLitExp $ SappExpLessThan $2 $3 }
    | "<=" Expression_ Expression_ { evaluateToLitExp $ SappExpLessThanOrEqualsTo $2 $3 }

Variable_
    : identifier_ { SappVar $1 }
{
parseError :: [Token] -> a
parseError tks = case tks of
    tk:_ -> error $ (prettyShow $ posn tk) ++ ": " ++ (prettyShow tk)
    [] -> error $ endOfFile ++ ": error"

-- XXX: This _optimization_ could be removed later
evaluateToLitExp :: SappExpression -> SappExpression
evaluateToLitExp integerExp = case integerExp of
    SappExpAddition (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitInteger (l + r)
    SappExpSubtraction (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitInteger (l - r)
    SappExpMultiplication (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitInteger (l * r)
    SappExpDivision (SappExpLitInteger l) (SappExpLitInteger r) | r /= 0 -> SappExpLitInteger (l `div` r)
    SappExpModulo (SappExpLitInteger l) (SappExpLitInteger r) | r /= 0  -> SappExpLitInteger (l `mod` r)
    SappExpExponentiation (SappExpLitInteger l) (SappExpLitInteger r) | r >= 0  -> SappExpLitInteger (l ^ r)
    SappExpIntNegation (SappExpLitInteger v) -> SappExpLitInteger (negate v)
    SappExpConjuction (SappExpLitBoolean l) (SappExpLitBoolean r) -> SappExpLitBoolean (l || r)
    SappExpDisjunction (SappExpLitBoolean l) (SappExpLitBoolean r) -> SappExpLitBoolean (l && r)
    SappExpNegation (SappExpLitBoolean v) -> SappExpLitBoolean (not v)
    SappExpEqualsTo (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitBoolean (l == r)
    SappExpDifferentFrom (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitBoolean (l /= r)
    SappExpGreaterThan (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitBoolean (l > r)
    SappExpGreaterThanOrEqualsTo (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitBoolean (l >= r)
    SappExpLessThan (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitBoolean (l < r)
    SappExpLessThanOrEqualsTo (SappExpLitInteger l) (SappExpLitInteger r) -> SappExpLitBoolean (l <= r)
    SappExpEqualsTo (SappExpLitBoolean l) (SappExpLitBoolean r) -> SappExpLitBoolean (l == r)
    SappExpDifferentFrom (SappExpLitBoolean l) (SappExpLitBoolean r) -> SappExpLitBoolean (l /= r)
    _ -> integerExp

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
    | SappExpSubtraction SappExpression SappExpression
    | SappExpMultiplication SappExpression SappExpression
    | SappExpDivision SappExpression SappExpression
    | SappExpModulo SappExpression SappExpression
    | SappExpExponentiation SappExpression SappExpression
    | SappExpIntNegation SappExpression
    | SappExpConjuction SappExpression SappExpression
    | SappExpDisjunction SappExpression SappExpression
    | SappExpNegation SappExpression
    | SappExpEqualsTo SappExpression SappExpression
    | SappExpDifferentFrom SappExpression SappExpression
    | SappExpGreaterThan SappExpression SappExpression
    | SappExpGreaterThanOrEqualsTo SappExpression SappExpression
    | SappExpLessThan SappExpression SappExpression
    | SappExpLessThanOrEqualsTo SappExpression SappExpression
    deriving (Show, Eq)

data SappVariable = SappVar String
    deriving (Show, Eq)

}
