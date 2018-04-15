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

import Control.Monad

import Lexer
import PrettyShow
import IndentedShow
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
    deriving (Eq)

instance Show SappStatement where
    show = indentShow

instance IndentedShow SappStatement where
    ishow stmt = do
        string "STATEMENT: "
        case stmt of
            SappStmtBlock stmts -> do
                string "BLOCK"
                indent $ forM_ stmts $ \stmt -> line >> ishow stmt

            SappStmtVariableDeclaration dt nam -> do
                string "DECALRE VARIABLE"
                indent $ do
                    string "DATA TYPE"
                    indent $ string (prettyShow dt)
                    line

                    string "NAME"
                    indent $ string nam
                    line

            SappStmtAssignment var exp -> do
                string "ASSIGNMENT"
                indent $ do
                    string "VARIABLE"
                    indent $ ishow var
                    line

                    string "EXPRESSION"
                    indent $ ishow exp
                    line

            SappStmtRead var -> do
                string "READ"
                indent $ ishow var
                line

            SappStmtWrite exps -> do
                string "WRITE"
                indent $
                    forM_ exps $ \eit -> do
                        case eit of
                            Left str -> do
                                string "STRING"
                                indent $ string (show str)
                            Right exp -> do
                                string "EXPRESSION"
                                indent $ ishow exp
                        line

            SappStmtIf exp thn Nothing -> do
                string "IF-THEN"
                indent $ do
                    string "CONDITION"
                    indent $ ishow exp
                    line

                    string "THEN"
                    indent $ ishow thn

            SappStmtIf exp thn (Just els) -> do
                string "IF-THEN-ELSE"
                indent $ do
                    string "CONDITION"
                    indent $ ishow exp
                    line

                    string "THEN"
                    indent $ ishow thn
                    line

                    string "ELSE"
                    indent $ ishow els

data SappDataType
    = SappDTInteger
    | SappDTBoolean
    deriving (Show, Eq)

instance PrettyShow SappDataType where
    prettyShow dt = case dt of
        SappDTInteger -> "integer"
        SappDTBoolean -> "boolean"

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

instance IndentedShow SappExpression where
    ishow exp = case exp of
        SappExpLitInteger num -> do
            string "INTEGER LITERAL"
            indent $ string (show num)
        SappExpLitBoolean bool -> do
            string "BOOLEAN LITERAL"
            indent $ string (if bool then "true" else "false")
        SappExpVariable var -> do
            string "VARIABLE ACCESS"
            indent $ ishow var
        SappExpAddition opl opr -> do
            string "ADDITION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpSubtraction opl opr -> do
            string "SUBTRACTION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpMultiplication opl opr -> do
            string "MULTIPLICATION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpDivision opl opr -> do
            string "DIVISION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpModulo opl opr -> do
            string "MODULO"
            indent (ishow opl)
            indent (ishow opr)
        SappExpExponentiation opl opr -> do
            string "EXPONENTIATION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpIntNegation op -> do
            string "INTEGER NEGATION"
            indent (ishow op)
        SappExpConjuction opl opr -> do
            string "CONJUCTION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpDisjunction opl opr -> do
            string "DISJUNCTION"
            indent (ishow opl)
            indent (ishow opr)
        SappExpNegation op -> do
            string "BOOLEAN NEGATION"
            indent (ishow op)
        SappExpEqualsTo opl opr -> do
            string "EQUALS TO"
            indent (ishow opl)
            indent (ishow opr)
        SappExpDifferentFrom opl opr -> do
            string "DIFFERENT FROM"
            indent (ishow opl)
            indent (ishow opr)
        SappExpGreaterThan opl opr -> do
            string "GREATER THAN"
            indent (ishow opl)
            indent (ishow opr)
        SappExpGreaterThanOrEqualsTo opl opr -> do
            string "GREATER THAN OR EQUALS TO"
            indent (ishow opl)
            indent (ishow opr)
        SappExpLessThan opl opr -> do
            string "LESS THAN"
            indent (ishow opl)
            indent (ishow opr)
        SappExpLessThanOrEqualsTo opl opr -> do
            string "LESS THAN OR EQUALS TO"
            indent (ishow opl)
            indent (ishow opr)

data SappVariable = SappVar String
    deriving (Show, Eq)

instance IndentedShow SappVariable where
    ishow var = case var of
        SappVar str -> string str

}
