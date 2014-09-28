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
    , tellLError
    , tellPError
    , alexGetPosition
    ) where

import           Error
import           Position
import           Lexeme

import           Control.Monad   (liftM)
import           Data.List       (intercalate, foldl')
import           Data.List.Split (splitOn)
import           Data.Sequence   (Seq, (|>), empty)

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

@idnid  = $small $idchar*
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
        @skip                   { tok' TkNewLine        }
        "end"                   { tok' TkEnd            }
        "return"                { tok' TkReturn         }
        ";"                     { tok' TkSemicolon      }
        ","                     { tok' TkComma          }

        -- -- Brackets
        "("                     { tok' TkLParen         }
        ")"                     { tok' TkRParen         }
        "["                     { tok' TkLBrackets      }
        "]"                     { tok' TkRBrackets      }

        -- Types
        "record"                { tok' TkRecordType     }
        "union"                 { tok' TkUnionType      }

        -- Statements
        -- -- Declarations
        "="                     { tok' TkAssign         }
        "def"                   { tok' TkDef            }
        "as"                    { tok' TkAs             }
        ":"                     { tok' TkSignature      }
        "->"                    { tok' TkArrow          }
        "."                     { tok' TkDot            }

        -- -- In/Out
        "read"                  { tok' TkRead           }
        "print"                 { tok' TkPrint          }

        -- -- Conditionals
        "if"                    { tok' TkIf             }
        "then"                  { tok' TkThen           }
        "elif"                  { tok' TkElif           }
        "else"                  { tok' TkElse           }

        "unless"                { tok' TkUnless         }

        "case"                  { tok' TkCase           }
        "when"                  { tok' TkWhen           }
        "otherwise"             { tok' TkOtherwise      }

        -- -- Loops
        "for"                   { tok' TkFor            }
        "in"                    { tok' TkIn             }
        ".."                    { tok' TkFromTo         }
        "do"                    { tok' TkDo             }

        "while"                 { tok' TkWhile          }
        "until"                 { tok' TkUntil          }
        "repeat"                { tok' TkRepeat         }

        "break"                 { tok' TkBreak          }
        "continue"              { tok' TkContinue       }

        -- Expressions/Operators
        -- -- Literals
        @int                    { tok  (TkInt . read)   }
        "true"                  { tok' (TkBool True)    }
        "false"                 { tok' (TkBool False)   }
        @float                  { tok  (TkFloat . read) }
        @char                   { tok  (TkChar . read)  }
        -- -- -- Filtering newlines
        @string                 { tok  (TkString . dropQuotationMarks 1 1 . backslash) }
        @multiline_string       { tok  (TkString . dropQuotationMarks 3 3 . backslash) }

        -- -- Arithmetic
        "+"                     { tok' TkPlus           }
        "-"                     { tok' TkMinus          }
        "*"                     { tok' TkTimes          }
        "/"                     { tok' TkDivide         }
        "%"                     { tok' TkModulo         }
        "^"                     { tok' TkPower          }

        -- -- Boolean
        "or"                    { tok' TkOr             }
        "and"                   { tok' TkAnd            }
        "not"                   { tok' TkNot            }

        "=="                    { tok' TkEqual          }
        "/="                    { tok' TkUnequal        }

        "<"                     { tok' TkLess           }
        ">"                     { tok' TkGreat          }
        "<="                    { tok' TkLessEq         }
        ">="                    { tok' TkGreatEq        }

        "@"                     { tok' TkBelongs        }

        -- -- String
        --"++"                    { tok' TkConcat         }

        -- -- Identifiers
        @idnid                  { tok TkIden            }
        @typeid                 { tok TkTypeId          }

        -- Errors
        .                       { tok (TkError . head)  }
        @string_error           { tok (TkStringError . dropQuotationMarks 1 0 . backslash) }
        @multiline_string_error { tok (TkStringError . dropQuotationMarks 3 0 . backslash) }

{

--------------------------------------------------------------------------------

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
    | TkConcat

    -- -- Identifiers
    | TkIden   { unTkIden   :: String }
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
        TkIden _        -> "variable identifier"
        TkTypeId _      -> "type identifier"
        TkEOF           -> "'EOF'"
        TkError _       -> "error on character '"
        TkStringError _ -> "error on string '"

--------------------------------------------------------------------------------

data AlexUserState = AlexUST { errors :: Seq Error }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUST empty

-- Some useful functions for the Alex monad (which is naturally an instance of state monad)
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let st = alex_ust s in Right (s {alex_ust = f st},())

getUserState :: Alex AlexUserState
getUserState = Alex (\s -> Right (s,alex_ust s))

tellLError :: Position -> LexerError -> Alex ()
tellLError posn err = modifyUserState $ \st -> st { errors = errors st |> (LError posn err) }

tellPError :: Position -> ParseError -> Alex ()
tellPError posn err = modifyUserState $ \st -> st { errors = errors st |> (PError posn err) }

----------------------------------------

backslash :: String -> String
backslash str = foldl' (flip replace) str chars
    where
        replace :: (Char, Char) -> String -> String
        replace (new, old) = intercalate [new] . splitOn ['\\', old]
        chars = [('\a', 'a'), ('\b', 'b'), ('\f', 'f'),
                 ('\n', 'n'), ('\r', 'r'), ('\t', 't'),
                 ('\v', 'v'), ('"', '"'), ('\\', '\\')]

dropQuotationMarks :: Int -> Int -> String -> String
dropQuotationMarks l r = reverse . drop r . reverse . drop l

toPosition :: AlexPosn -> Position
toPosition (AlexPn _ r c) = Posn (r, c)

alexEOF :: Alex (Lexeme Token)
alexEOF = liftM (Lex TkEOF) alexGetPosition

-- Unfortunately, we have to extract the matching bit of string ourselves
tok :: (String -> Token) -> AlexAction (Lexeme Token)
tok f (p,_,_,s) i = return $ Lex (f $ take i s) (toPosition p)

-- For constructing tokens that do not depend on the input
tok' :: Token -> AlexAction (Lexeme Token)
tok' = tok . const

alexGetPosition :: Alex Position
alexGetPosition = alexGetInput >>= \(p,_,_,_) -> return $ toPosition p

runAlex' :: String -> Alex a -> (Seq Error, a)
runAlex' input (Alex f) =
    let Right (st,a) = f state
        ust = errors (alex_ust st)
    in (ust,a)
    where
        state :: AlexState
        state = AlexState
            { alex_pos   = alexStartPos
            , alex_inp   = input
            , alex_chr   = '\n'
            , alex_bytes = []
            , alex_ust   = alexInitUserState
            , alex_scd   = 0
            }

}
