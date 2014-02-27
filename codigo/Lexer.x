{
{-| Lexer for programming language SUPERCOOL.
-}

module Lexer ( Alex(..)
             , AlexPosn(..)
             , Token(..)
             , alexMonadScan
             , runAlex
             , alexGetInput
             ) where

import Prelude hiding (lex)
--import System.IO (readFile)
--import System.Environment (getArgs)
}

%wrapper "monad"

$newline   = [\n\r]
@spaces    = ($white # $newline)|\\$newline

$digit = 0-9

$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha = [$small $large]

$idchar = [$alpha $digit]

@varid    = $small $idchar*
@typeid = $large $idchar*

@int    = $digit+
@float  = $digit+(\.$digit+)?
@string = \"($printable # [\"\\]|\\$printable)*\"

-------------------------------------------------------------------------------

tokens :-

        -- Whitespace/Comments
        @spaces+        ;
        "--".*          ;

        -- Language
        $newline        { lex' TkNewLine       }
        "main"          { lex' TkMain          }
        "begin"         { lex' TkBegin         }
        "end"           { lex' TkEnd           }
        "return"        { lex' TkReturn        }
        ";"             { lex' TkSemicolon     }
        ","             { lex' TkComma         }

        -- -- Brackets
        "("             { lex' TkLParen        }
        ")"             { lex' TkRParen        }
        "["             { lex' TkLBrackets     }
        "]"             { lex' TkRBrackets     }
        "{"             { lex' TkLBraces       }
        "}"             { lex' TkRBraces       }

        -- Types
        "Void"          { lex' TkVoidType      }
        "Int"           { lex' TkIntType       }
        "Bool"          { lex' TkBoolType      }
        "Float"         { lex' TkFloatType     }
        "Char"          { lex' TkCharType      }
        "String"        { lex' TkStringType    }
        "Range"         { lex' TkRangeType     }
        "Union"         { lex' TkUnionType     }
        "Record"        { lex' TkRecordType    }
        "Type"          { lex' TkTypeType      }

        -- Statements
        -- -- Declarations
        "="             { lex' TkAssign        }
        "def"           { lex' TkDef           }
        "as"            { lex' TkAs            }
        "::"            { lex' TkSignature     }
        "->"            { lex' TkArrow         }

        -- -- In/Out
        "read"          { lex' TkRead          }
        "print"         { lex' TkPrint         }

        -- -- Conditionals
        "if"            { lex' TkIf            }
        "then"          { lex' TkThen          }
        "else"          { lex' TkElse          }

        "unless"        { lex' TkUnless        }

        "case"          { lex' TkCase          }
        "when"          { lex' TkWhen          }
        --":"             { lex' TkColon         }

        -- -- Loops
        "for"           { lex' TkFor           }
        "in"            { lex' TkIn            }
        ".."            { lex' TkFromTo        }
        "do"            { lex' TkDo            }

        "while"         { lex' TkWhile         }
        "until"         { lex' TkUntil         }

        "break"         { lex' TkBreak         }
        "continue"      { lex' TkContinue      }

        -- Expressions/Operators
        -- -- Literals
        @int            { lex (TkInt . read)   }
        "true"          { lex' (TkTrue True)   }
        "false"         { lex' (TkFalse False) }
        @float          { lex (TkFloat . read) }
        @string         { lex TkString         }

        -- -- Num
        "+"             { lex' TkPlus          }
        "-"             { lex' TkMinus         }
        "*"             { lex' TkTimes         }
        "/"             { lex' TkDivide        }
        "%"             { lex' TkModulo        }
        "^"             { lex' TkPower         }

        -- -- Bool
        "or"            { lex' TkOr            }
        "and"           { lex' TkAnd           }
        "not"           { lex' TkNot           }

        "=="            { lex' TkEqual         }
        "/="            { lex' TkUnequal       }

        "<"             { lex' TkLess          }
        ">"             { lex' TkGreat         }
        "<="            { lex' TkLessEq        }
        ">="            { lex' TkGreatEq       }

        -- -- Identifiers
        @varid          { lex TkVarId          }
        @typeid         { lex TkTypeId         }

{

-------------------------------------------------------------------------------

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
    | TkInt Int
    | TkFloat Float
    | TkString String
    | TkTrue Bool
    | TkFalse Bool
    -- -- Num
    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkPower
    -- -- Bool
    | TkOr | TkAnd | TkNot
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq
    -- -- Identifiers
    | TkVarId String
    | TkTypeId String
    | TkEOF             -- TEMPORAL (o no??)
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
showPosn (AlexPn _ line col) = show line ++ ':' : show col

--main :: IO ()
--main = do
--    args <- getArgs
--    str <- if null args
--        then getContents
--        else readFile (head args)
--    case scanner str of
--        Right lexs -> mapM_ print lexs
--        Left error -> print error
}
























--{
--{-| Lexer for programming language SUPERCOOL.
---}

--module Main where

--import System.IO (readFile)
--import System.Environment (getArgs)
--import Data.Maybe (fromJust, isJust)
--import Control.Monad (when)
--}

--%wrapper "monad"

--$newline   = [\n\r]
--@spaces    = ($white # $newline)|\\$newline

--$digit = 0-9

--$large = [A-Z \xc0-\xd6 \xd8-\xde]
--$small = [a-z \xdf-\xf6 \xf8-\xff \_]
--$alpha = [$small $large]

--$idchar = [$alpha $digit]

--@varid    = $small $idchar*
--@recordid = $large $idchar*

--@int    = $digit+
--@float  = $digit+(\.$digit+)?
--@string = \"($printable # [\"\\]|\\$printable)*\"


--tokens :-

--    -- Whitespace/Comments
--    @spaces+        ;
--    "--".*          ;

--    -- Language
--    $newline+       { mkLex TkNewLine   }
--    main            { mkLex TkMain      }
--    begin           { mkLex TkBegin     }
--    end             { mkLex TkEnd       }
--    return          { mkLex TkReturn    }
--    \;              { mkLex TkSemicolon }
--    \,              { mkLex TkComma     }

--    -- -- Brackets
--    \(              { mkLex TkLParen    }
--    \)              { mkLex TkRParen    }
--    \[              { mkLex TkLBrackets }
--    \]              { mkLex TkRBrackets }
--    \{              { mkLex TkLBraces   }
--    \}              { mkLex TkRBraces   }


--    -- Types
--    Void            { mkLex TkVoidType   }
--    Int             { mkLex TkIntType    }
--    Bool            { mkLex TkBoolType   }
--    Float           { mkLex TkFloatType  }
--    Char            { mkLex TkCharType   }
--    String          { mkLex TkStringType }
--    Range           { mkLex TkRangeType  }
--    Union           { mkLex TkUnionType  }
--    Record          { mkLex TkRecordType }
--    Type            { mkLex TkTypeType   }

--    -- Statements
--    -- -- Declarations
--    \=              { mkLex TkAssign    }
--    def             { mkLex TkDef       }
--    as              { mkLex TkAs        }
--    "::"            { mkLex TkSignature }
--    "->"            { mkLex TkArrow     }
--    \.              { mkLex TkDot}

--    -- -- In/Out
--    read            { mkLex TkRead  }
--    print           { mkLex TkPrint }

--    -- -- Conditionals
--    if              { mkLex TkIf   }
--    then            { mkLex TkThen }
--    else            { mkLex TkElse }

--    unless          { mkLex TkUnless }

--    case            { mkLex TkCase  }
--    when            { mkLex TkWhen  }
--    --":"             { mkLex TkColon }

--    -- -- Loops
--    for             { mkLex TkFor    }
--    in              { mkLex TkIn     }
--    ".."            { mkLex TkFromTo }
--    do              { mkLex TkDo     }

--    while           { mkLex TkWhile }
--    until           { mkLex TkUntil }

--    break           { mkLex TkBreak    }
--    continue        { mkLex TkContinue }

--    -- Expressions/Operators
--    -- -- Literals
--    @int            { mkLex TkInt    }
--    true            { mkLex TkTrue   }
--    false           { mkLex TkFalse  }
--    @float          { mkLex TkFloat  }
--    @string         { mkLex TkString }

--    -- -- Num
--    \+              { mkLex TkPlus   }
--    \-              { mkLex TkMinus  }
--    \*              { mkLex TkTimes  }
--    \/              { mkLex TkDivide }
--    \%              { mkLex TkModulo }
--    \^              { mkLex TkPower  }

--    -- -- Bool
--    or              { mkLex TkOr  }
--    and             { mkLex TkAnd }
--    not             { mkLex TkNot }

--    "=="            { mkLex TkEqual   }
--    "/="            { mkLex TkUnequal }

--    \<              { mkLex TkLess    }
--    \>              { mkLex TkGreat   }
--    "<="            { mkLex TkLessEq  }
--    ">="            { mkLex TkGreatEq }

--    -- -- Identifiers
--    @varid          { mkLex TkVarId    }
--    @recordid       { mkLex TkRecordId }

--{
--data Lexeme = Lex AlexPosn Token String

--instance Show Lexeme where
--    show (Lex p TkNewLine s) =
--        showPosn p ++ " -> " ++ show TkNewLine ++ " : " ++ show (length s) ++ " `\\n`"
--    show (Lex p TkSemicolon s) =
--        showPosn p ++ " -> " ++ show TkSemicolon ++ " : " ++ show (length s)  ++ " `;`"
--    show (Lex p t s) = showPosn p ++ " -> " ++ show t ++ ": `" ++ s ++ "`"

--data Token
--    -- Language
--    = TkNewLine | TkMain | TkBegin | TkEnd | TkReturn | TkSemicolon | TkComma
--    -- -- Brackets
--    | TkLParen | TkRParen | TkLBrackets | TkRBrackets | TkLBraces | TkRBraces
--    -- Types
--    | TkVoidType | TkIntType | TkBoolType | TkFloatType | TkCharType
--    | TkStringType | TkRangeType | TkUnionType | TkRecordType | TkTypeType
--    -- Statements
--    -- -- Declarations
--    | TkAssign | TkDef | TkAs | TkSignature | TkArrow | TkDot
--    -- -- In/Out
--    | TkRead | TkPrint
--    -- -- Conditionals
--    | TkIf | TkThen | TkElse
--    | TkUnless
--    | TkCase | TkWhen
--    -- -- Loops
--    | TkFor | TkIn | TkFromTo | TkDo
--    | TkWhile | TkUntil
--    | TkBreak | TkContinue
--    -- Expressions/Operators
--    -- -- Literals
--    | TkInt | TkTrue | TkFalse | TkFloat | TkString
--    -- -- Num
--    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkPower
--    -- -- Bool
--    | TkOr | TkAnd | TkNot
--    | TkEqual | TkUnequal
--    | TkLess | TkGreat | TkLessEq | TkGreatEq
--    -- -- Identifiers
--    | TkVarId | TkRecordId
--    | TkEOF             -- TEMPORAL
--    deriving (Eq, Show)

--mkLex :: Token -> AlexInput -> Int -> Alex Lexeme
--mkLex tok (pos,_,_,input) len = return $ Lex pos tok (take len input)

-- --FUNCION TEMPORAL
----lexError :: a -> Alex a
----lexError str = do
----    (pos, _, _, input) <- alexGetInput
----    alexError $ showPosn pos ++ ": " ++ str ++
----        if (not (null input))
----            then " before " ++ show (head input)
--            --else " at end of file"

------ FUNCION TEMPORAL
----scanner :: String -> String -> Either String [Lexeme]
----scanner str err =
----    let loop err = do
----        (t, m) <- alexComplementError alexMonadScan
----        case m of
----            Just newErr -> do
----                skip str 1
----                loop $ newErr ++ err
----            otherwise   -> do
----                let tok@(Lex _ cl _) = t
----                if (cl == TkEOF)
----                    then
----                        if null err
----                            then return [tok]
----                            else fail err
----                    else do
----                        toks <- loop err
----                        return (tok : toks)
----    in  runAlex str $ loop err

------ we capture the error message in order to complement it with the file position
----alexComplementError :: Alex a -> Alex (a, Maybe String)
----alexComplementError (Alex al) =
----    Alex (\s -> case al s of
----        Right (s', x) -> Right (s', (x, Nothing))
----        Left  message -> Right (s, (undefined, Just message)))



------ FUNCION TEMPORAL
----scanner :: String -> Either String [Lexeme]
----scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
----                            when (isJust m) (lexError (fromJust m))
----                            let tok@(Lex _ cl _) = t
----                            if (cl == TkEOF)
----                               then return [tok]
----                               else do toks <- loop
----                                       return (tok : toks)
----              in  runAlex str loop

------ we capture the error message in order to complement it with the file position
----alexComplementError :: Alex a -> Alex (a, Maybe String)
----alexComplementError (Alex al) = Alex (\s -> case al s of
----                                                 Right (s', x) -> Right (s', (x, Nothing))
----                                                 Left  message -> Right (s, (undefined, Just message)))

------ TEMPORAL
--alexEOF :: Alex Lexeme
--alexEOF = do
--    (pos, _, _, _) <- alexGetInput
--    return $ Lex pos TkEOF ""

---- FUNCION TEMPORAL
--scanner :: String -> Either String [Lexeme]
--scanner str = runAlex str loop
--    where loop = do
--            lex@(Lex _ tok _) <- alexMonadScan
--            if tok == TkEOF
--                then return [lex]
--                else do
--                    lexs <- loop
--                    return (lex:lexs)

----alexEOF = return TkEOF

--showPosn :: AlexPosn -> String
--showPosn (AlexPn _ line col) = show line ++ ':' : show col

---- runhaskell Lexer.hs <file>
--main :: IO ()
--main = do
--    args <- getArgs
--    str <- if null args
--        then getContents
--        else readFile (head args)
--    case scanner str of
--        Right lexs -> mapM_ print lexs
--        Left error -> print error
--}
