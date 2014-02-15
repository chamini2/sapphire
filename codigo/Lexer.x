{- | Lexer for programming language SUPERCOOL.
-}

{
module Lexer () where
}

%wrapper "basic"

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$symbol = [\%\*\+\^\-\.\/\<\=\>\|]   -- reconsiderar |, depende de array

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$idchar    = [$alpha $digit \_]
$symchar   = [$symbol \:]
$nl        = [\n\r]

@reservedid =
    Int

tokens :-

    $white+             ;   -- whitespaces
    "--".*              ;   -- comments

    -- data types
    Int                 { \s -> Int }
    Float               { \s -> Float }
    Char                { \s -> Char }
    String              { \s -> String }
    ------ arrays

    -- literals
    $digit+

data Token
    = Int Int
    | Float Float
    | Char
    | String
