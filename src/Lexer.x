{
module Lexer
    ( scanTokens
    ) where
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
    "or" { pushToken TkOr }
    "and" { pushToken TkAnd }
    "not" { pushToken TkNot }
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
    | TkOr AlexPosn
    | TkAnd AlexPosn
    | TkNot AlexPosn
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

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
