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

import           Checker       (CheckError (LError), LexerError (..))
import           Language      (Position, Lexeme (..))

import           Prelude       hiding (lex, null)
import           Control.Monad (liftM)
import           Data.Sequence (Seq, (|>), empty, null)

}

%wrapper "monadUserState"

$newline   = [\n\r]
@spaces    = ($white # $newline)|\\$newline

@skip = [\; $white]+

$digit = 0-9

$large = [A-Z]
$small = [a-z \_]
$alpha = [$small $large]

$idchar = [$alpha $digit]

@varid  = $small $idchar*
@typeid = $large $idchar*

@int         = $digit+
@float       = $digit+(\.$digit+)?
@string      = \"(($printable # [\"\\])|\\[abfnrtv]|\\$newline)*\"
@stringerror = \"(($printable # [\"\\])|\\[abfnrtv]|\\$newline)*
@char        = \'$printable\'

--------------------------------------------------------------------------------

tokens :-

        -- Whitespace/Comments
        @spaces+        ;
        "#".*           ;

        -- Language
        @skip           { lex' TkNewLine        }
        "end"           { lex' TkEnd            }
        "return"        { lex' TkReturn         }
        ";"             { lex' TkSemicolon      }
        ","             { lex' TkComma          }

        -- -- Brackets
        "("             { lex' TkLParen         }
        ")"             { lex' TkRParen         }
        "["             { lex' TkLBrackets      }
        "]"             { lex' TkRBrackets      }
        --"{"             { lex' TkLBraces        }
        --"}"             { lex' TkRBraces        }

        -- Types
        --"Void"          { lex' TkVoidType       }
        "Int"           { lex' TkIntType        }
        "Bool"          { lex' TkBoolType       }
        "Float"         { lex' TkFloatType      }
        "Char"          { lex' TkCharType       }
        "String"        { lex' TkStringType     }
        "Range"         { lex' TkRangeType      }
        "Union"         { lex' TkUnionType      }
        "Record"        { lex' TkRecordType     }
        "Type"          { lex' TkTypeType       }

        -- Statements
        -- -- Declarations
        "="             { lex' TkAssign         }
        "def"           { lex' TkDef            }
        "imp"           { lex' TkImp            }
        "as"            { lex' TkAs             }
        "::"            { lex' TkSignature      }
        "->"            { lex' TkArrow          }
        "."             { lex' TkDot            }

        -- -- In/Out
        "read"          { lex' TkRead           }
        "print"         { lex' TkPrint          }

        -- -- Conditionals
        "if"            { lex' TkIf             }
        "then"          { lex' TkThen           }
        "elif"          { lex' TkElif           }
        "else"          { lex' TkElse           }

        "unless"        { lex' TkUnless         }

        "case"          { lex' TkCase           }
        "when"          { lex' TkWhen           }
        "otherwise"     { lex' TkOtherwise      }

        -- -- Loops
        "for"           { lex' TkFor            }
        "in"            { lex' TkIn             }
        ".."            { lex' TkFromTo         }
        "do"            { lex' TkDo             }

        "while"         { lex' TkWhile          }
        "until"         { lex' TkUntil          }
        "repeat"        { lex' TkRepeat         }

        "break"         { lex' TkBreak          }
        "continue"      { lex' TkContinue       }

        -- Expressions/Operators
        -- -- Literals
        @int            { lex  (TkInt . read)   }
        "true"          { lex' (TkBool True)    }
        "false"         { lex' (TkBool False)   }
        @float          { lex  (TkFloat . read) }
        @char           { lex  (TkChar . read)  }
        -- -- -- Filtering newlines
        @string         { lex  (TkString . init . tail . filterNewline) }

        -- -- Num
        "+"             { lex' TkPlus           }
        "-"             { lex' TkMinus          }
        "*"             { lex' TkTimes          }
        "/"             { lex' TkDivide         }
        "%"             { lex' TkModulo         }
        "^"             { lex' TkPower          }

        -- -- Bool
        "or"            { lex' TkOr             }
        "and"           { lex' TkAnd            }
        "not"           { lex' TkNot            }

        "@"             { lex' TkBelongs        }

        "=="            { lex' TkEqual          }
        "/="            { lex' TkUnequal        }

        "<"             { lex' TkLess           }
        ">"             { lex' TkGreat          }
        "<="            { lex' TkLessEq         }
        ">="            { lex' TkGreatEq        }

        -- -- Identifiers
        @varid          { lex TkVarId           }
        @typeid         { lex TkTypeId          }

        -- Errors
        .               { lex (TkError . head) }
        @stringerror    { lex (TkStringError . tail . filterNewline) }

{

--------------------------------------------------------------------------------

data Token

    -- Language
    = TkNewLine | TkEnd | TkReturn | TkSemicolon | TkComma

    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets {-| TkLBraces | TkRBraces-}

    -- Types
    {-| TkVoidType-} | TkIntType | TkBoolType | TkFloatType | TkCharType
    | TkStringType | TkRangeType | TkUnionType | TkRecordType | TkTypeType

    -- Statements
    -- -- Declarations
    | TkAssign | TkDef | TkImp | TkAs | TkSignature | TkArrow | TkDot

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
    | TkBelongs
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq

    -- -- Identifiers
    | TkVarId  { unTkVarId  :: String }
    | TkTypeId { unTkTypeId :: String }

    -- Compiling
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
        --TkLBraces       -> "'{'"
        --TkRBraces       -> "'}'"
        --TkVoidType      -> "type 'Void'"
        TkIntType       -> "type 'Int'"
        TkBoolType      -> "type 'Bool'"
        TkFloatType     -> "type 'Float'"
        TkCharType      -> "type 'Char'"
        TkStringType    -> "type 'String'"
        TkRangeType     -> "type 'Range'"
        TkUnionType     -> "type 'Union'"
        TkRecordType    -> "type 'Record'"
        TkTypeType      -> "type 'Type'"
        TkAssign        -> "'='"
        TkDef           -> "'def'"
        TkImp           -> "'imp'"
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
        TkBelongs       -> "'@'"
        TkEqual         -> "'=='"
        TkUnequal       -> "'/='"
        TkLess          -> "'<'"
        TkGreat         -> "'>'"
        TkLessEq        -> "'<='"
        TkGreatEq       -> "'>='"
        TkVarId _       -> "variable identifier"
        TkTypeId _      -> "type identifier"
        TkEOF           -> "'EOF'"
        TkError _       -> "error on character '"
        TkStringError _ -> "error on string '"

--------------------------------------------------------------------------------

data AlexUserState = AlexUSt { lexerErrors :: Seq CheckError }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUSt empty

-- some useful interaces to the Alex monad (which is naturally an instance of state monad)
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

filterNewline :: String -> String
filterNewline = foldr func []
    where
        func c str = case (c,str) of
            ('\\','\n':'\r':strs) -> strs
            ('\\','\r':'\n':strs) -> strs
            ('\\',     '\n':strs) -> strs
            ('\\',      'a':strs) -> '\a' : strs
            ('\\',      'b':strs) -> '\b' : strs
            ('\\',      'f':strs) -> '\f' : strs
            ('\\',      'n':strs) -> '\n' : strs
            ('\\',      'r':strs) -> '\r' : strs
            ('\\',      't':strs) -> '\t' : strs
            ('\\',      'v':strs) -> '\v' : strs
            _                     -> c    : str

toPosition :: AlexPosn -> Position
toPosition (AlexPn _ line col) = (line,col)

alexEOF :: Alex (Lexeme Token)
alexEOF = liftM (Lex TkEOF) alexGetPosn

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> Token) -> AlexAction (Lexeme Token)
lex f (p,_,_,s) i = return $ Lex (f $ take i s) (toPosition p)

-- For constructing tokens that do not depend on
-- the input
lex' :: Token -> AlexAction (Lexeme Token)
lex' = lex . const

alexGetPosn :: Alex Position
alexGetPosn = alexGetInput >>= \(p,_,_,_) -> return $ toPosition p

runAlex' :: String -> Alex a -> (Seq CheckError, a)
runAlex' input (Alex f) =
    let Right (st,a) = f state
        ust = lexerErrors (alex_ust st)
    in (ust,a)
    where
        state = AlexState
            { alex_pos = alexStartPos
            , alex_inp = input
            , alex_chr = '\n'
            , alex_bytes = []
            , alex_ust = alexInitUserState
            , alex_scd = 0
            }

}
