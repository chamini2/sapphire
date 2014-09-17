{
{-|
    Lexer for programming language SAPPHIRE.
-}

module Lexer
    ( Alex (..)
    , Token (..)
    , Lexeme (..)
    , AlexUserState (..)
    , alexMonadScan
    , runAlex'
    , addLexerError
    , alexGetPosn
    ) where

import           Error           (Error (LError), LexerError (..))
import           Position
import           Lexeme

import           Control.Monad   (liftM)
import           Data.List       (intercalate, foldl')
import           Data.List.Split (splitOn)
import           Data.Sequence   (Seq, (|>), empty, null)
import           Prelude         hiding (lex, null)

}

%wrapper "monadUserState"

$newline = [\n\r]

@spaces  = ($white # $newline)

@skip = [\; $white]+

$digit = 0-9

$large = [A-Z]
$small = [a-z \_]
$alpha = [$small $large]

$idchar = [$alpha $digit]

-- Alex won't work if we write it directly in the @inside_string
$backslash = ["\\abfnrtv]

@inside_string          = ($printable # ["\\] | \\$backslash)
@inside_multilinestring = (@inside_string | $newline )

@varid  = $small $idchar*
@typeid = $large $idchar*

@int    = $digit+
@float  = $digit+(\.$digit+)?
@char   = \'$printable\'

@string                 = \"@inside_string*\"
@string_error           = \"@inside_string*
@multiline_string       = \"\"\"(@inside_multilinestring | \"@inside_multilinestring | \"\"@inside_multilinestring)*\"\"\"
@multiline_string_error = \"\"\"(@inside_multilinestring | \"@inside_multilinestring | \"\"@inside_multilinestring)*

--------------------------------------------------------------------------------

tokens :-

        -- Whitespace/Comments
        @spaces+        ;
        "#".*           ;

        -- Language
        @skip                   { lex' TkNewLine        }
        "end"                   { lex' TkEnd            }
        "return"                { lex' TkReturn         }
        ";"                     { lex' TkSemicolon      }
        ","                     { lex' TkComma          }

        -- -- Brackets
        "("                     { lex' TkLParen         }
        ")"                     { lex' TkRParen         }
        "["                     { lex' TkLBrackets      }
        "]"                     { lex' TkRBrackets      }

        -- Types
        --"Int"                   { lex' TkIntType        }
        --"Float"                 { lex' TkFloatType      }
        --"Bool"                  { lex' TkBoolType       }
        --"Char"                  { lex' TkCharType       }
        --"String"                { lex' TkStringType     }
        --"Range"                 { lex' TkRangeType      }
        --"Type"                  { lex' TkTypeType       }
        "record"                { lex' TkRecordType     }
        "union"                 { lex' TkUnionType      }

        -- Statements
        -- -- Declarations
        "="                     { lex' TkAssign         }
        "def"                   { lex' TkDef            }
        "as"                    { lex' TkAs             }
        ":"                     { lex' TkSignature      }
        "->"                    { lex' TkArrow          }
        "."                     { lex' TkDot            }

        -- -- In/Out
        "read"                  { lex' TkRead           }
        "print"                 { lex' TkPrint          }

        -- -- Conditionals
        "if"                    { lex' TkIf             }
        "then"                  { lex' TkThen           }
        "elif"                  { lex' TkElif           }
        "else"                  { lex' TkElse           }

        "unless"                { lex' TkUnless         }

        "case"                  { lex' TkCase           }
        "when"                  { lex' TkWhen           }
        "otherwise"             { lex' TkOtherwise      }

        -- -- Loops
        "for"                   { lex' TkFor            }
        "in"                    { lex' TkIn             }
        ".."                    { lex' TkFromTo         }
        "do"                    { lex' TkDo             }

        "while"                 { lex' TkWhile          }
        "until"                 { lex' TkUntil          }
        "repeat"                { lex' TkRepeat         }

        "break"                 { lex' TkBreak          }
        "continue"              { lex' TkContinue       }

        -- Expressions/Operators
        -- -- Literals
        @int                    { lex  (TkInt . read)   }
        "true"                  { lex' (TkBool True)    }
        "false"                 { lex' (TkBool False)   }
        @float                  { lex  (TkFloat . read) }
        @char                   { lex  (TkChar . read)  }
        -- -- -- Filtering newlines
        @string                 { lex  (TkString . dropQuotationMarks 1 1 . backslash) }
        @multiline_string       { lex  (TkString . dropQuotationMarks 3 3 . backslash) }

        -- -- Arithmetic
        "+"                     { lex' TkPlus           }
        "-"                     { lex' TkMinus          }
        "*"                     { lex' TkTimes          }
        "/"                     { lex' TkDivide         }
        "%"                     { lex' TkModulo         }
        "^"                     { lex' TkPower          }

        -- -- Boolean
        "or"                    { lex' TkOr             }
        "and"                   { lex' TkAnd            }
        "not"                   { lex' TkNot            }

        "=="                    { lex' TkEqual          }
        "/="                    { lex' TkUnequal        }

        "<"                     { lex' TkLess           }
        ">"                     { lex' TkGreat          }
        "<="                    { lex' TkLessEq         }
        ">="                    { lex' TkGreatEq        }

        "@"                     { lex' TkBelongs        }

        -- -- String
        --"++"                    { lex' TkConcat         }

        -- -- Identifiers
        @varid                  { lex TkVarId           }
        @typeid                 { lex TkTypeId          }

        -- Errors
        .                       { lex (TkError . head)  }
        @string_error           { lex (TkStringError . dropQuotationMarks 1 0 . backslash) }
        @multiline_string_error { lex (TkStringError . dropQuotationMarks 3 0 . backslash) }

{

--------------------------------------------------------------------------------

data Token

    -- Language
    = TkNewLine | TkEnd | TkReturn | TkSemicolon | TkComma

    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets

    -- Types
--    | TkIntType | TkFloatType | TkBoolType | TkCharType
--    | TkStringType | TkRangeType | TkTypeType
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
    | TkConcat

    -- -- Identifiers
    | TkVarId  { unTkVarId  :: String }
    | TkTypeId { unTkTypeId :: String }

    -- Compiler
    | TkEOF
    | TkError       { unTkError       :: Char   }
    | TkStringError { unTkStringError :: String }
    deriving (Eq)

instance Show Token where
    show tk = case tk of
        TkNewLine       -> "'newline'"
        TkEnd           -> "'end'"
        TkReturn        -> "'return'"
        TkSemicolon     -> "';'"
        TkComma         -> "','"
        TkLParen        -> "'('"
        TkRParen        -> "')'"
        TkLBrackets     -> "'['"
        TkRBrackets     -> "']'"
        --TkIntType       -> "type 'Int'"
        --TkFloatType     -> "type 'Float'"
        --TkBoolType      -> "type 'Bool'"
        --TkCharType      -> "type 'Char'"
        --TkStringType    -> "type 'String'"
        --TkRangeType     -> "type 'Range'"
        --TkTypeType      -> "type 'Type'"
        TkRecordType    -> "type 'record'"
        TkUnionType     -> "type 'union'"
        TkAssign        -> "'='"
        TkDef           -> "'def'"
        TkAs            -> "'as'"
        TkSignature     -> "'::'"
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
        TkVarId _       -> "variable identifier"
        TkTypeId _      -> "type identifier"
        TkEOF           -> "'EOF'"
        TkError _       -> "error on character '"
        TkStringError _ -> "error on string '"

--------------------------------------------------------------------------------

data AlexUserState = AlexUSt { lexerErrors :: Seq Error }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUSt empty

-- Some useful functions for the Alex monad (which is naturally an instance of state monad)
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let st = alex_ust s in Right (s {alex_ust = f st},())

getUserState :: Alex AlexUserState
getUserState = Alex (\s -> Right (s,alex_ust s))

addLexerError :: Lexeme Token -> Alex ()
addLexerError (Lex t p) = do
    let err = case t of
            TkError c       -> LError p $ UnexpectedChar c
            TkStringError s -> LError p $ StringError s
    modifyUserState $ \st -> st { lexerErrors = lexerErrors st |> err }

----------------------------------------

backslash :: String -> String
backslash str = foldl' (flip (uncurry replace)) str chars
    where
        replace :: Char -> Char -> String -> String
        replace new old = intercalate [new] . splitOn ['\\', old]
        chars = [('\a', 'a'), ('\b', 'b'), ('\f', 'f'),
                 ('\n', 'n'), ('\r', 'r'), ('\t', 't'),
                 ('\v', 'v'), ('"', '"'), ('\\', '\\')]

dropQuotationMarks :: Int -> Int -> String -> String
dropQuotationMarks l r = reverse . drop r . reverse . drop l

toPosition :: AlexPosn -> Position
toPosition (AlexPn _ row col) = Posn (row, col)

alexEOF :: Alex (Lexeme Token)
alexEOF = liftM (Lex TkEOF) alexGetPosn

-- Unfortunately, we have to extract the matching bit of string ourselves...
lex :: (String -> Token) -> AlexAction (Lexeme Token)
lex f (p,_,_,s) i = return $ Lex (f $ take i s) (toPosition p)

-- For constructing tokens that do not depend on the input
lex' :: Token -> AlexAction (Lexeme Token)
lex' = lex . const

alexGetPosn :: Alex Position
alexGetPosn = alexGetInput >>= \(p,_,_,_) -> return $ toPosition p

runAlex' :: String -> Alex a -> (Seq Error, a)
runAlex' input (Alex f) =
    let Right (st,a) = f state
        ust = lexerErrors (alex_ust st)
    in (ust,a)
    where
        state :: AlexState
        state = AlexState
            { alex_pos = alexStartPos
            , alex_inp = input
            , alex_chr = '\n'
            , alex_bytes = []
            , alex_ust = alexInitUserState
            , alex_scd = 0
            }

}
