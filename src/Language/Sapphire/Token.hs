{-# LANGUAGE LambdaCase #-}
module Language.Sapphire.Token
    ( Token (..)
    ) where

data Token

    -- Language
    = TkNewLine | TkEnd | TkReturn | TkSemicolon | TkComma

    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets

    -- Types
    | TkRecordType | TkUnionType

    -- Statements
    -- -- Declarations
    | TkAssign | TkDef | TkAs | TkSignature | TkArrow | TkDot

    -- -- In/Out
    | TkRead | TkPrint

    -- -- Conditionals
    | TkIf | TkThen | TkElif | TkElse
    | TkUnless
    | TkCase | TkWhen | TkOtherwise

    -- -- Loops
    | TkFor | TkIn | TkFromTo | TkDo
    | TkWhile | TkUntil | TkRepeat
    | TkBreak | TkContinue

    -- Expressions/Operators
    -- -- Literals
    | TkInt    { unTkInt    :: Int    }
    | TkFloat  { unTkFloat  :: Float  }
    | TkString { unTkString :: String }
    | TkChar   { unTkChar   :: Char   }
    | TkBool   { unTkBool   :: Bool   }

    -- -- Num
    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkPower

    -- -- Bool
    | TkOr | TkAnd | TkNot
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq
    | TkBelongs

    -- -- String
--    | TkConcat

    -- -- Identifiers
    | TkIden   { unTkIden   :: String }
    | TkTypeId { unTkTypeId :: String }

    -- Compiler
    | TkEOF
    | TkError       { unTkError       :: Char   }
    | TkStringError { unTkStringError :: String }
    deriving (Eq)

instance Show Token where
    show = \case
        TkNewLine       -> "'newline'"
        TkEnd           -> "'end'"
        TkReturn        -> "'return'"
        TkSemicolon     -> "';'"
        TkComma         -> "','"
        TkLParen        -> "'('"
        TkRParen        -> "')'"
        TkLBrackets     -> "'['"
        TkRBrackets     -> "']'"
        TkRecordType    -> "type 'record'"
        TkUnionType     -> "type 'union'"
        TkAssign        -> "'='"
        TkDef           -> "'def'"
        TkAs            -> "'as'"
        TkSignature     -> "':'"
        TkArrow         -> "'->'"
        TkDot           -> "'.'"
        TkRead          -> "'read'"
        TkPrint         -> "'print'"
        TkIf            -> "'if'"
        TkThen          -> "'then'"
        TkElif          -> "'elif'"
        TkElse          -> "'else'"
        TkUnless        -> "'unless'"
        TkCase          -> "'case'"
        TkWhen          -> "'when'"
        TkOtherwise     -> "'otherwise'"
        TkFor           -> "'for'"
        TkIn            -> "'in'"
        TkFromTo        -> "'..'"
        TkDo            -> "'do'"
        TkWhile         -> "'while'"
        TkUntil         -> "'until'"
        TkRepeat        -> "'repeat'"
        TkBreak         -> "'break'"
        TkContinue      -> "'continue'"
        TkInt _         -> "literal 'int'"
        TkFloat _       -> "literal 'float'"
        TkString _      -> "literal 'string'"
        TkChar _        -> "literal 'char'"
        TkBool _        -> "literal 'bool'"
        TkPlus          -> "'+'"
        TkMinus         -> "'-'"
        TkTimes         -> "'*'"
        TkDivide        -> "'/'"
        TkModulo        -> "'%'"
        TkPower         -> "'^'"
        TkOr            -> "'or'"
        TkAnd           -> "'and'"
        TkNot           -> "'not'"
        TkEqual         -> "'=='"
        TkUnequal       -> "'/='"
        TkLess          -> "'<'"
        TkGreat         -> "'>'"
        TkLessEq        -> "'<='"
        TkGreatEq       -> "'>='"
        TkBelongs       -> "'@'"
        --TkConcat        -> "'++'"
        TkIden _        -> "variable identifier"
        TkTypeId _      -> "type identifier"
        TkEOF           -> "'EOF'"
        TkError _       -> "error on character '"
        TkStringError _ -> "error on string '"
