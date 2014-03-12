{-# LANGUAGE GADTs #-}
module Language where

import           Prelude
import qualified Data.Sequence as DS

--type Program = StFunction
newtype Program = Program (DS.Seq Statement)
    deriving (Show)

type Identifier = String

data DataType = Void | Int | Float | Bool | Char | String | Range | Type-- | Array
    deriving (Show, Eq)

----------------------------------------

data Statement where
    -- Language
    StNoop   :: Statement
    StAssign :: Identifier -> Expression -> Statement
    -- Definitions
    StDeclaration :: DS.Seq Declaration -> Statement
    StReturn      :: Expression    -> Statement
    -- I/O
    StRead  :: DS.Seq Identifier -> Statement
    StPrint :: [Expression] -> Statement
    -- Conditional
    StIf   :: Expression -> [Statement] -> [Statement] -> Statement
    StCase :: Expression -> [Case]      -> [Statement] -> Statement
    -- Loops
    StWhile    :: Expression -> DS.Seq Statement ->  Statement
    StFor      :: Identifier -> Expression  -> DS.Seq Statement -> Statement
    StBreak    :: Statement
    StContinue :: Statement
    deriving (Show)


data Declaration = Declaration Identifier DataType Category
    deriving (Show)

data Category = CatVariable
              | CatFunction
              | CatParameter
              | CatRecordField
              | CatUnionField
              | CatDataType             -- Que es esto?? no será CatDeclaration? En verdad no tengo idea
              deriving (Eq, Show)

data Case = Case Expression (DS.Seq Statement)
    deriving (Show)

----------------------------------------

data Expression where
    -- Variable
    Variable :: Identifier -> Expression
    -- Literals
    LitInt    :: Int    -> Expression
    LitFloat  :: Float  -> Expression
    LitBool   :: Bool   -> Expression
    LitChar   :: Char   -> Expression
    LitString :: String -> Expression
    --LitRange  :: Range  -> Expression
    -- Operators
    ExpBinary :: Binary   -> Expression -> Expression {- -> DataType -} -> Expression
    ExpUnary  :: Unary    -> Expression -> Expression {- -> DataType -}
    --ExpArray  :: ExpressionArray
    deriving (Show)

data Range = FromTo Expression Expression
    deriving (Show)

data Binary
    = OpPlus  | OpMinus | OpTimes | OpDivide | OpModulo | OpPower | OpFromTo
    | OpOr    | OpAnd
    | OpEqual | OpUnEqual | OpLess | OpLessEq | OpGreat | OpGreatEq | OpBelongs
    deriving (Show)

binaryOperation :: Binary -> [((DataType, DataType), DataType)]
binaryOperation op = case op of
    OpPlus    -> zip numbers [Int, Float]
    OpMinus   -> zip numbers [Int, Float]
    OpTimes   -> zip numbers [Int, Float]
    OpDivide  -> zip numbers [Int, Float]
    OpModulo  -> zip numbers [Int, Float]
    OpPower   -> zip numbers [Int, Float]
    OpFromTo  -> [((Int,Int),Range)]
    OpOr      -> [((Bool,Bool),Bool)]
    OpAnd     -> [((Bool,Bool),Bool)]
    OpEqual   -> ((Bool,Bool),Bool) : zip numbers [Bool, Bool]
    OpUnEqual -> ((Bool,Bool),Bool) : zip numbers [Bool, Bool]
    OpLess    -> zip numbers [Bool, Bool]
    OpLessEq  -> zip numbers [Bool, Bool]
    OpGreat   -> zip numbers [Bool, Bool]
    OpGreatEq -> zip numbers [Bool, Bool]
    OpBelongs -> zip numbers [Bool, Bool]
    where
        numbers = [(Int,Int), (Float,Float)]

data Unary = OpNegate | OpNot
    deriving (Show)

unaryOperation :: Unary -> [(DataType, DataType)]
unaryOperation op = case op of
    OpNegate -> [(Int, Int), (Float, Float)]
    OpNot    -> [(Bool, Bool)]
