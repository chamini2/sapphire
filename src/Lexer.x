{
module Lexer
    ( scanTokens
    , Token(..)
    , posn
    ) where

import PrettyShow
}

%wrapper "posn"

$digit = 0-9
$letter = [_A-Za-z]

@string_char = $printable # [\"\\] | \\ [\"\\tn]

-----

tokens :-
    -- Skipped chars
    $white+ ; -- do nothing
    -- End-of-line comments
    "#".* ; -- do nothing

    -- Program
    "main" { pushToken TkMain }
    "end" { pushToken TkEnd }

    -- Statements
    ";" { pushToken TkSemicolon }
    ":=" { pushToken TkAssign }
    "begin" { pushToken TkBegin }
    "read" { pushToken TkRead }
    "write" { pushToken TkWrite }
    "if" { pushToken TkIf }
    "then" { pushToken TkThen }
    "else" { pushToken TkElse }

    -- Expression Literals
    \"@string_char*\" { consumeToken (TkLitCharString . read) } -- "
    $digit+ { consumeToken (TkLitInteger . read) }
    "true" { pushToken (TkLitBoolean True) }
    "false" { pushToken (TkLitBoolean False) }

    -- Operators
    "+" { pushToken TkAddition }
    "-" { pushToken TkSubtraction }
    "*" { pushToken TkMultiplication }
    "/" { pushToken TkDivision }
    "%" { pushToken TkModulo }
    "^" { pushToken TkExponentiation }
    "or" { pushToken TkDisjunction }
    "and" { pushToken TkConjuction }
    "not" { pushToken TkNegation }
    "=" { pushToken TkEqualsTo }
    "/=" { pushToken TkDifferentFrom }
    ">" { pushToken TkGreaterThan }
    ">=" { pushToken TkGreaterThanOrEquals }
    "<" { pushToken TkLessThan }
    "<=" { pushToken TkLessThanOrEquals }

    -- Data types
    "integer" { pushToken TkInteger }
    "boolean" { pushToken TkBoolean }

    -- Identifiers
    $letter[$letter $digit]* { consumeToken TkIdentifier }

{
pushToken :: (AlexPosn -> Token) -> AlexPosn -> String -> Token
pushToken tok posn _ = tok posn

consumeToken :: (String -> AlexPosn -> Token) -> AlexPosn -> String -> Token
consumeToken tok posn inp = tok inp posn

scanTokens :: String -> [Token]
scanTokens = alexScanTokens

posn :: Token -> AlexPosn
posn tok = case tok of
    TkMain p -> p
    TkEnd p -> p
    TkSemicolon p -> p
    TkAssign p -> p
    TkBegin p -> p
    TkRead p -> p
    TkWrite p -> p
    TkIf p -> p
    TkThen p -> p
    TkElse p -> p
    TkLitCharString _ p -> p
    TkLitInteger _ p -> p
    TkLitBoolean _ p -> p
    TkAddition p -> p
    TkSubtraction p -> p
    TkMultiplication p -> p
    TkDivision p -> p
    TkModulo p -> p
    TkExponentiation p -> p
    TkDisjunction p -> p
    TkConjuction p -> p
    TkNegation p -> p
    TkEqualsTo p -> p
    TkDifferentFrom p -> p
    TkGreaterThan p -> p
    TkGreaterThanOrEquals p -> p
    TkLessThan p -> p
    TkLessThanOrEquals p -> p
    TkInteger p -> p
    TkBoolean p -> p
    TkIdentifier _ p -> p

data Token
    = TkMain AlexPosn
    | TkEnd AlexPosn
    | TkSemicolon AlexPosn
    | TkAssign AlexPosn
    | TkBegin AlexPosn
    | TkRead AlexPosn
    | TkWrite AlexPosn
    | TkIf AlexPosn
    | TkThen AlexPosn
    | TkElse AlexPosn
    | TkLitCharString String AlexPosn
    | TkLitInteger Integer AlexPosn
    | TkLitBoolean Bool AlexPosn
    | TkAddition AlexPosn
    | TkSubtraction AlexPosn
    | TkMultiplication AlexPosn
    | TkDivision AlexPosn
    | TkModulo AlexPosn
    | TkExponentiation AlexPosn
    | TkDisjunction AlexPosn
    | TkConjuction AlexPosn
    | TkNegation AlexPosn
    | TkEqualsTo AlexPosn
    | TkDifferentFrom AlexPosn
    | TkGreaterThan AlexPosn
    | TkGreaterThanOrEquals AlexPosn
    | TkLessThan AlexPosn
    | TkLessThanOrEquals AlexPosn
    | TkInteger AlexPosn
    | TkBoolean AlexPosn
    | TkIdentifier String AlexPosn
    deriving (Show, Eq)

instance PrettyShow Token where
    prettyShow tok = case tok of
        TkMain _ -> "main"
        TkEnd _ -> "end"
        TkSemicolon _ -> ";"
        TkAssign _ -> ":="
        TkBegin _ -> "begin"
        TkRead _ -> "read"
        TkWrite _ -> "write"
        TkIf _ -> "if"
        TkThen _ -> "then"
        TkElse _ -> "else"
        TkLitCharString v _ -> v
        TkLitInteger v _ -> show v
        TkLitBoolean v _ -> if v then "true" else "false"
        TkAddition _ -> "+"
        TkSubtraction _ -> "-"
        TkMultiplication _ -> "*"
        TkDivision _ -> "/"
        TkModulo _ -> "%"
        TkExponentiation _ -> "^"
        TkDisjunction _ -> "or"
        TkConjuction _ -> "and"
        TkNegation _ -> "not"
        TkEqualsTo _ -> "="
        TkDifferentFrom _ -> "/="
        TkGreaterThan _ -> ">"
        TkGreaterThanOrEquals _ -> ">="
        TkLessThan _ -> "<"
        TkLessThanOrEquals _ -> ">="
        TkInteger _ -> "integer"
        TkBoolean _ -> "boolean"
        TkIdentifier v _ -> v

instance PrettyShow AlexPosn where
    prettyShow (AlexPn abs lin col) = "(" ++ show lin ++ ":" ++ show col ++ ")"

}
