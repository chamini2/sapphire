{
{-| Lexer for programming language SAPPHIRE.
-}

module Lexer
    ( Alex(..)
    , AlexPosn(..)
    , Token(..)
    , alexMonadScan
    , runAlex
    , alexGetInput
    , showPosn
    ) where

import           Prelude hiding (lex)
}

%wrapper "monad"

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
@string = \"($printable # [\"\\]|\\$printable)*\"
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
        .               { lex (TkError . head)  }

{

--------------------------------------------------------------------------------

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
    deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return TkEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: a -> AlexAction a
lex' = lex . const

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':' : show (col - 1) ++ ": "

}
