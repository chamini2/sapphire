{
{-| Lexer for programming language SUPERCOOL.
-}

module Lexer where

import System.IO (readFile)
import System.Environment (getArgs)
}

%wrapper "monad"

$white   = [ \t]
$newline = [\n\r\f\v]

$digit = 0-9

$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha = [$small $large]

$idchar = [$alpha $digit]

@varid    = $small $idchar*
@structid = $large $idchar*

@int    = $digit+
@float  = $digit+(\.$digit+)?
@string = \"($printable # \")*\"

tokens :-

    -- Whitespace/Comments
    $white+         ;
    "--".*          ;

    -- Language
    $newline        { mkLex TkNewLine }
    main            { mkLex TkMain }
    begin           { mkLex TkBegin }
    end             { mkLex TkEnd }
    return          { mkLex TkReturn }
    \;              { mkLex TkSemicolon }
    \,              { mkLex TkComma }

    -- -- Brackets
    \(              { mkLex TkLParen }
    \)              { mkLex TkRParen }
    \[              { mkLex TkLBrackets }
    \]              { mkLex TkRBrackets }
    --\{              { mkLex TkLBraces }
    --\}              { mkLex TkRBraces }


    -- Types
    Void            { mkLex TkVoidType }
    Int             { mkLex TkIntType }
    Bool            { mkLex TkBoolType}
    Float           { mkLex TkFloatType }
    Char            { mkLex TkCharType }
    String          { mkLex TkStringType }
    Range           { mkLex TkRangeType }
    Union           { mkLex TkUnion }
    Struct          { mkLex TkStruct }

    -- Statements
    -- -- Declarations
    \=              { mkLex TkAssign }
    def             { mkLex TkDef }
    as              { mkLex TkAs}
    "::"            { mkLex TkSignature }
    "->"            { mkLex TkArrow }

    -- -- In/Out
    read            { mkLex TkRead }
    print           { mkLex TkPrint }

    -- -- Conditionals
    if              { mkLex TkIf }
    then            { mkLex TkThen }
    else            { mkLex TkElse }

    unless          { mkLex TkUnless }

    case            { mkLex TkCase }
    when            { mkLex TkWhen }
    --":"             { mkLex TkColon }

    -- -- Loops
    for             { mkLex TkFor }
    in              { mkLex TkIn }
    ".."            { mkLex TkFromTo }
    do              { mkLex TkDo }

    while           { mkLex TkWhile }
    until           { mkLex TkUntil}

    break           { mkLex TkBreak }
    continue        { mkLex TkContinue }

    -- Expressions/Operators
    -- -- Literals
    @int            { mkLex TkInt }
    true            { mkLex TkTrue }
    false           { mkLex TkFalse }
    @float          { mkLex TkFloat }
    @string         { mkLex TkString }

    -- -- Num
    \+              { mkLex TkPlus }
    \-              { mkLex TkMinus }
    \*              { mkLex TkTimes }
    \/              { mkLex TkDivide }
    \%              { mkLex TkModulo }
    \^              { mkLex TkPower }

    -- -- Bool
    or              { mkLex TkOr }
    and             { mkLex TkAnd }
    not             { mkLex TkNot }

    "=="            { mkLex TkEqual }
    "/="            { mkLex TkUnequal }

    \<              { mkLex TkLess }
    \>              { mkLex TkGreat }
    "<="            { mkLex TkLessEq }
    ">="            { mkLex TkGreatEq }

    -- -- Functions
    toInt           { mkLex TkToInt }
    toFloat         { mkLex TkToFloat }
    toString        { mkLex TkToString }
    length          { mkLex TkLength }

    -- -- Identifiers
    @varid          { mkLex TkVarId }
    @structid       { mkLex TkStructId }

{
data Lexeme = Lex AlexPosn Token String deriving Show

data Token
    -- Language
    = TkNewLine | TkMain | TkBegin | TkEnd | TkReturn | TkSemicolon | TkComma
    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets
    -- Types
    | TkVoidType | TkIntType | TkBoolTyp | TkFloatType | TkCharType | TkStringType | TkRangeType | TkUnion | TkStruct
    -- Statements
    -- -- Declarations
    | TkAssign | TkDef | TkA | TkSignature | TkArrow
    -- -- In/Out
    | TkRead | TkPrint
    -- -- Conditionals
    | TkIf | TkThen | TkElse
    | TkUnless
    | TkCase | TkWhen
    -- -- Loops
    | TkFor | TkIn | TkFromTo | TkDo
    | TkWhile | TkUnti
    | TkBreak | TkContinue
    -- Expressions/Operators
    -- -- Literals
    | TkInt | TkTrue | TkFalse | TkFloat | TkString
    -- -- Num
    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkPower
    -- -- Bool
    | TkOr | TkAnd | TkNot
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq
    -- -- Functions
    | TkToInt | TkToFloat | TkToString | TkLength
    -- -- Identifiers
    | TkVarId | TkStructId
    | TkEOF             -- TEMPORAL
    deriving (Eq, Show)

mkLex :: Token -> AlexInput -> Int -> Alex Lexeme
mkLex tok (pos,_,_,input) len = return $ Lex pos tok (take len input)

-- FUNCION TEMPORAL
lexError str = do
    (pos, _, _, input) <- alexGetInput
    alexError $ showPosn pos ++ ": " ++ str ++
        (if (not (null input))
            then " before " ++ show (head input)
            else " at end of file")

-- FUNCION TEMPORAL
scanner str = runAlex str $ do
    let loop = do
        lex@(Lex _ tok _) <- alexMonadScan
        if tok == TkEOF
            then return [lex]
            else do
                lexs <- loop
                return (lex:lexs)
    loop

-- TEMPORAL
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    return $ Lex pos TkEOF ""

showPosn (AlexPn _ line col) = show line ++ ':': show col

-- runhaskell Lexer.hs
-- luego escribir codigo de SUPERCOOL en la consola
-- al finalizar, hacer <ctrl+D>
main = do
    args <- getArgs
    str <- if null args
        then getContents
        else readFile (head args)
    case scanner str of
        Right lexs -> mapM_ print lexs
        Left error -> print error
}
