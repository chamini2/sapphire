{
{-| Lexer for programming language SUPERCOOL.
-}

module Lexer where
}

%wrapper "monad"

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
    \;              { mkLex TkSemicolon }

    -- -- Brackets
    \(              { mkLex TkLParen }
    \)              { mkLex TkRParen }
    \[              { mkLex TkLBrackets }
    \]              { mkLex TkRBrackets }
    \{              { mkLex TkLBraces }
    \}              { mkLex TkRBraces }


    -- Types
    Void            { mkLex TkVoidType }
    Int             { mkLex TkIntType }
    Bool            { mkLex TkBoolType}
    Float           { mkLex TkFloatType }
    Char            { mkLex TkCharType }
    String          { mkLex TkStringType }
    Union           { mkLex TkUnion }
    Struct          { mkLex TkStruct }
    Range           { mkLex TkStruct }

    -- Instructions
    -- -- Declarations
    \=              { mkLex TkAssign }
    def             { mkLex TkDef }
    \,              { mkLex TkComma }
    "::"            { mkLex TkSignature }
    "->"            { mkLex TkArrow }

    -- -- In/Out
    read            { mkLex TkRead }
    write           { mkLex TkWrite }

    -- -- Conditionals
    if              { mkLex TkIf }
    else            { mkLex TkElse }

    case            { mkLex TkCase }
    of              { mkLex TkOf }
    end             { mkLex TkEnd }
    ":"             { mkLex TkColon }

    -- -- Loops
    for             { mkLex TkFor }
    in              { mkLex TkIn }
    ".."

    while           { mkLex TkWhile }

    break           { mkLex TkBreak }
    continue        { mkLex TkContinue }

    -- Expressions/Operators
    -- -- Identifiers
    @varid          { mkLex TkVarId }
    @structid       { mkLex TkStructId }

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

{
data Lexeme = Lex AlexPosn Token String deriving Show

data Token
    = TkSemicolon
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets | TkLBraces | TkRBraces
    | TkVoidType | TkIntType | TkBoolType | TkFloatType | TkCharType 
    | TkStringType | TkRangeType | TkUnion | TkStruct
    | TkAssign
    | TkDef | TkComma | TkSignature | TkArrow
    | TkRead | TkWrite
    | TkIf | TkElse | TkCase | TkOf | TkEnd | TkColon
    | TkFor | TkIn | TkWhile | TkBreak | TkContinue
    | TkVarId | TkStructId
    | TkInt | TkTrue | TkFalse | TkFloat | TkString
    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkPower
    | TkOr | TkAnd | TkNot
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq
    | TkToInt | TkToFloat | TkToString
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
                lexemes <- loop
                return (lex:lexemes)
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
  str <- getContents
  print (scanner str)
}
