{-# LANGUAGE DeriveDataTypeable #-}
module Expression
    ( Expression(..)
    , Binary(..)
    , Unary(..)
    , binaryOperation
    , unaryOperation
    )where

import           Lexeme
import           DataType
import           Identifier

import qualified Data.Data              as DD (Data)
import           Data.Sequence          as DS (Seq, fromList)
import qualified Data.Typeable          as DT (Typeable)

data Expression
    -- Variable
--    = Variable (Lexeme Access)
    -- Function call
    = FunctionCall (Lexeme Identifier) (Seq (Lexeme Expression))
    -- Literals
    | LitInt    (Lexeme Int)
    | LitFloat  (Lexeme Float)
    | LitBool   (Lexeme Bool)
    | LitChar   (Lexeme Char)
    | LitString (Lexeme String)
--    | LitRange  (Lexeme Range)
    -- Operators
    | ExpBinary (Lexeme Binary) (Lexeme Expression) (Lexeme Expression)
    | ExpUnary  (Lexeme Unary)  (Lexeme Expression)
    deriving (Eq, Ord, DT.Typeable, DD.Data, Show)

--instance Show Expression where
--    show = runPrinter . printExpression

data Binary
    = OpPlus  | OpMinus   | OpTimes | OpDivide | OpModulo | OpPower   | OpFromTo
    | OpEqual | OpUnequal | OpLess  | OpLessEq | OpGreat  | OpGreatEq | OpBelongs
    | OpOr    | OpAnd
    deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show Binary where
    show op = case op of
        OpPlus    -> "+"
        OpMinus   -> "-"
        OpTimes   -> "*"
        OpDivide  -> "/"
        OpModulo  -> "%"
        OpPower   -> "^"
        OpFromTo  -> ".."
        OpOr      -> "or"
        OpAnd     -> "and"
        OpEqual   -> "=="
        OpUnequal -> "/="
        OpLess    -> "<"
        OpLessEq  -> "<="
        OpGreat   -> ">"
        OpGreatEq -> ">="
        OpBelongs -> "@"

--instance Show Binary where
--    show op = case op of
--        OpPlus    -> "arithmetic addition"
--        OpMinus   -> "arithmetic substraction"
--        OpTimes   -> "arithmetic multiplication"
--        OpDivide  -> "arithmetic division"
--        OpModulo  -> "arithmetic Modulo"
--        OpPower   -> "arithmetic power"
--        OpFromTo  -> "range construction operator"
--        OpOr      -> "logical disjunction"
--        OpAnd     -> "logical conjunction"
--        OpEqual   -> "equal to"
--        OpUnequal -> "not equal to"
--        OpLess    -> "less than"
--        OpLessEq  -> "less than or equal to"
--        OpGreat   -> "greater than"
--        OpGreatEq -> "greater than or equal to"
--        OpBelongs -> "belongs to Range"

binaryOperation :: Binary -> Seq ((DataType, DataType), DataType)
binaryOperation op = fromList $ case op of
    OpPlus    -> arithmetic
    OpMinus   -> arithmetic
    OpTimes   -> arithmetic
    OpDivide  -> arithmetic
    OpModulo  -> arithmetic
    OpPower   -> [ ((Int, Int), Int), ((Float, Int), Float) ]
    OpFromTo  -> [ ((Int, Int), Range) ]
    OpOr      -> boolean
    OpAnd     -> boolean
    OpEqual   -> everything
    OpUnequal -> everything
    OpLess    -> arithmeticCompare
    OpLessEq  -> arithmeticCompare
    OpGreat   -> arithmeticCompare
    OpGreatEq -> arithmeticCompare
    OpBelongs -> [ ((Int, Range), Bool) ]
    where
        everything = arithmetic ++ boolean
        arithmetic = [((Int, Int), Int), ((Float, Float), Float)]
        boolean = [ ((Bool, Bool), Bool) ]
        arithmeticCompare = zip numbers $ repeat Bool
        numbers = [(Int, Int), (Float, Float)]

data Unary = OpNegate | OpNot
    deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show Unary where
    show op = case op of
        OpNegate -> "-"
        OpNot    -> "not"

--instance Show Unary where
--    show op = case op of
--        OpNegate -> "arithmetic negation"
--        OpNot    -> "logical negation"

unaryOperation :: Unary -> Seq (DataType, DataType)
unaryOperation op = fromList $ case op of
    OpNegate -> [(Int, Int), (Float, Float)]
    OpNot    -> [(Bool, Bool)]

