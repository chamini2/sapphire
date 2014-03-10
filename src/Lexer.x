{
{-| Lexer for programming language SAPPHIRE.
-}

module Lexer
    ( Alex(..)
    , AlexPosn(..)
    , Token(..)
    , Lexeme(..)
    , AlexUserState(..)
    , alexMonadScan
    , runAlex'
    , addLexerError
    , alexGetPosn
    , alexShowPosn
    ) where

import           Checker

import           Prelude hiding (lex)
}

--%wrapper "monad"
%wrapper "monadUserState"

$newline   = [\n\r]
@spaces    = ($white # $newline)|\\$newline

@skip = [\; $white]+

$digit = 0-9

--$large = [A-Z \xc0-\xd6 \xd8-\xde]
--$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$large = [A-Z]
$small = [a-z \_]
$alpha = [$small $large]

$idchar = [$alpha $digit]

@varid    = $small $idchar*
@typeid = $large $idchar*

@int    = $digit+
@float  = $digit+(\.$digit+)?
@string = \"($printable # [\"\\]|\\$printable|\\$newline)*\"
@stringerror = \"($printable # [\"\\]|\\$printable|\\$newline)*
@char   = \'$printable\'

--------------------------------------------------------------------------------

tokens :-

        -- Whitespace/Comments
        @spaces+        ;
        "--".*          ;

        -- Language
        --$newline        { lex' TkNewLine        }
        @skip           { lex' TkNewLine        }
        "main"          { lex' TkMain           }
        "begin"         { lex' TkBegin          }
        "end"           { lex' TkEnd            }
        "return"        { lex' TkReturn         }
        ";"             { lex' TkSemicolon      }
        ","             { lex' TkComma          }

        -- -- Brackets
        "("             { lex' TkLParen         }
        ")"             { lex' TkRParen         }
        "["             { lex' TkLBrackets      }
        "]"             { lex' TkRBrackets      }
        "{"             { lex' TkLBraces        }
        "}"             { lex' TkRBraces        }

        -- Types
        "Void"          { lex' TkVoidType       }
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
        "as"            { lex' TkAs             }
        "::"            { lex' TkSignature      }
        "->"            { lex' TkArrow          }

        -- -- In/Out
        "read"          { lex' TkRead           }
        "print"         { lex' TkPrint          }

        -- -- Conditionals
        "if"            { lex' TkIf             }
        "then"          { lex' TkThen           }
        "else"          { lex' TkElse           }

        "unless"        { lex' TkUnless         }

        "case"          { lex' TkCase           }
        "when"          { lex' TkWhen           }

        -- -- Loops
        "for"           { lex' TkFor            }
        "in"            { lex' TkIn             }
        ".."            { lex' TkFromTo         }
        "do"            { lex' TkDo             }

        "while"         { lex' TkWhile          }
        "until"         { lex' TkUntil          }

        "break"         { lex' TkBreak          }
        "continue"      { lex' TkContinue       }

        -- Expressions/Operators
        -- -- Literals
        @int            { lex  (TkInt . read)   }
        "true"          { lex' (TkTrue True)    }
        "false"         { lex' (TkFalse False)  }
        @float          { lex  (TkFloat . read) }
        @string         { lex  TkString         }
        @char           { lex  (TkChar . read)  }

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
        .               { lex  (TkError . head) }
        @stringerror    { lex TkStringError     }

{

--------------------------------------------------------------------------------

data Lexeme = Lex Token AlexPosn deriving (Eq, Show)

data Token

    -- Language
    = TkNewLine | TkMain | TkBegin | TkEnd | TkReturn | TkSemicolon | TkComma

    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets | TkLBraces | TkRBraces

    -- Types
    | TkVoidType | TkIntType | TkBoolType | TkFloatType | TkCharType
    | TkStringType | TkRangeType | TkUnionType | TkRecordType | TkTypeType

    -- Statements
    -- -- Declarations
    | TkAssign | TkDef | TkAs | TkSignature | TkArrow | TkDot

    -- -- In/Out
    | TkRead | TkPrint

    -- -- Conditionals
    | TkIf | TkThen | TkElse
    | TkUnless
    | TkCase | TkWhen

    -- -- Loops
    | TkFor | TkIn | TkFromTo | TkDo
    | TkWhile | TkUntil
    | TkBreak | TkContinue

    -- Expressions/Operators
    -- -- Literals
    | TkInt    Int
    | TkFloat  Float
    | TkString String
    | TkChar   Char
    | TkTrue   Bool
    | TkFalse  Bool

    -- -- Num
    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkPower

    -- -- Bool
    | TkOr | TkAnd | TkNot
    | TkBelongs
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq

    -- -- Identifiers
    | TkVarId String
    | TkTypeId String

    -- Compiling
    | TkEOF
    | TkError Char
    | TkStringError String
    deriving (Eq, Show)



--------------------------------------------------------------------------------

data AlexUserState = AlexUSt { lexerErrors :: [(LexerError, Lexeme)] }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUSt []

-- some useful interaces to the Alex monad (which is naturally an instance of state monad)
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let st = alex_ust s in Right (s {alex_ust = f st},())

getUserState :: Alex AlexUserState
getUserState = Alex (\s -> Right (s,alex_ust s))

--pushLexerError :: Token -> Alex ()
--pushLexerError t = alexGetPosn >>= \p -> modifyUserState (push $ Lex t p)
--    where
--       push :: Lexeme -> AlexUserState -> AlexUserState
--       push l (AlexUSt ls) = AlexUSt $ l : ls


addLexerError :: Token -> Alex ()
addLexerError t = do
    p <- alexGetPosn
    let error = case t of
            TkError c       -> (UnexpectedChar c, Lex t p)
            TkStringError s -> (StringError s   , Lex t p)
    Alex $ \s -> let st = alex_ust s in Right (s { alex_ust = st { lexerErrors = error : (lexerErrors st) } } , () )

----------------------------------------

--alexEOF :: Alex Lexeme
--alexEOF = alexGetPosn >>= return . Lex TkEOF
alexEOF :: Alex Token
alexEOF = return TkEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
--lex :: (String -> Token) -> AlexAction Lexeme
--lex f = \(p,_,_,s) i -> return $ Lex (f $ take i s) p
lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

-- For constructing tokens that do not depend on
-- the input
--lex' :: Token -> AlexAction Lexeme
--lex' = lex . const
lex' :: a -> AlexAction a
lex' = lex . const

alexGetPosn :: Alex AlexPosn
alexGetPosn = alexGetInput >>= \(p,_,_,_) -> return p

alexShowPosn :: AlexPosn -> String
alexShowPosn (AlexPn _ line col) = show line ++ ':' : show (col - 1) ++ ": "

runAlex' :: String -> Alex a -> Either [(LexerError,Lexeme)] a
runAlex' input (Alex f)
   = case f (AlexState
            { alex_pos = alexStartPos
            , alex_inp = input
            , alex_chr = '\n'
            , alex_bytes = []
            , alex_ust = alexInitUserState
            , alex_scd = 0 }) of
                Right (st,a) ->
                    let ust = lexerErrors (alex_ust st)
                    in if null ust
                        then Right a
                        else Left ust
}