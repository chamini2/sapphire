{
{-# OPTIONS_GHC -w #-}
{-|
    Lexer for the programming language sapphire
-}

module Language.Sapphire.Lexer
    ( Alex (..)
    , Token (..)
    , Lexeme (..)
    , alexMonadScan
    , runAlex'
    , tellLError
    , tellPError
    ) where

import           Language.Sapphire.Error
import           Language.Sapphire.Lexeme
import           Language.Sapphire.Token
import           Language.Sapphire.SappMonad (initialWriter)

import           Control.Monad            (liftM)
import           Data.List                (intercalate, foldl')
import           Data.List.Split          (splitOn)
import           Data.Sequence            (Seq, (|>), empty)

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

-- Alex complains if is written directly in the `@inside_string`
$backslash = ["\\abfnrtv]

@inside_string          = ($printable # ["\\] | \\$backslash)
@inside_multilinestring = (@inside_string | $newline )

@ident  = $small $idchar*
@typeid = $large $idchar*

@int    = $digit+
@float  = $digit+(\.$digit+)?
@char   = \'($printable # ['\\] | \\' | \\$backslash)\'

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
        "main"                  { tok' TkMain           }
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
        "this"                  { tok' TkThis           }

        "break"                 { tok' TkBreak          }
        "continue"              { tok' TkContinue       }

        -- Expressions/Operators
        -- -- Literals
        @int                    { tok  (TkInt . read)   }
        "true"                  { tok' (TkBool True)    }
        "false"                 { tok' (TkBool False)   }
        @float                  { tok  (TkFloat . read) }
        -- -- -- Filtering newlines and escaped characters
        @char                   { tok  (TkChar . read . backslash) }
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
        "++"                    { tok' TkConcat         }

        -- -- Identifiers
        @ident                  { tok TkIden            }
        @typeid                 { tok TkTypeId          }

        -- Errors
        .                       { tok (TkError . head)  }
        @string_error           { tok (TkStringError . dropQuotationMarks 1 0 . backslash) }
        @multiline_string_error { tok (TkStringError . dropQuotationMarks 3 0 . backslash) }

{

--------------------------------------------------------------------------------

data AlexUserState = AlexUST { errors :: Seq Error }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUST initialWriter

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

runAlex' :: String -> Alex a -> (a, Seq Error)
runAlex' input (Alex f) =
    let Right (st,a) = f state
        ust = errors (alex_ust st)
    in (a,ust)
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
