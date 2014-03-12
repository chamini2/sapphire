{-# LANGUAGE GADTs #-}
module Language where

import           Prelude
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Identity
import qualified Data.Foldable as DF (mapM_, foldr)
import           Data.Sequence as DS (Seq, singleton)

--type Program = StFunction
newtype Program = Program (Seq Statement)
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
    StDeclaration :: Seq Declaration -> Statement
    StReturn      :: Expression      -> Statement
    -- I/O
    StRead  :: Seq Identifier -> Statement
    StPrint :: Seq Expression -> Statement
    -- Conditional
    StIf   :: Expression -> Seq Statement -> Seq Statement -> Statement
    StCase :: Expression -> Seq Case      -> Seq Statement -> Statement
    -- Loops
    StWhile    :: Expression -> Seq Statement ->     Statement
    StFor      :: Identifier -> Expression    -> Seq Statement -> Statement
    StBreak    :: Statement
    StContinue :: Statement

instance Show Statement where show = runPrinter . printStatement

data Declaration = Declaration Identifier DataType Category
    deriving (Show)

data Category = CatVariable
              | CatFunction
              | CatParameter
              | CatRecordField
              | CatUnionField
              | CatDataType             -- Que es esto?? R: Es para cuando se declaran los tipos primitivos en la tabla de simbolos
              deriving (Eq, Show)

data Case = Case Expression (Seq Statement)
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

instance Show Expression where show = runPrinter . printExpression

data Range = FromTo Expression Expression
    deriving (Show)

data Binary
    = OpPlus  | OpMinus | OpTimes | OpDivide | OpModulo | OpPower | OpFromTo
    | OpOr    | OpAnd
    | OpEqual | OpUnequal | OpLess | OpLessEq | OpGreat | OpGreatEq | OpBelongs

instance Show Binary where
    show op = case op of
        OpPlus    -> "'Arithmetic addition'"
        OpMinus   -> "'Arithmetic substraction'"
        OpTimes   -> "'Arithmetic multiplication'"
        OpDivide  -> "'Arithmetic division'"
        OpModulo  -> "'Arithmetic Modulo'"
        OpPower   -> "'Arithmetic power'"
        OpFromTo  -> "'Range construction operator'"
        OpOr      -> "'Logical disjunction'"
        OpAnd     -> "'Logical conjunction'"
        OpEqual   -> "'Equal to'"
        OpUnequal -> "'Not equal to'"
        OpLess    -> "'Less than'"
        OpLessEq  -> "'Less than or equal to'"
        OpGreat   -> "'Greater than'"
        OpGreatEq -> "'Greater than or equal to'"
        OpBelongs -> "'Belongs to Range'"

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
    OpUnequal -> ((Bool,Bool),Bool) : zip numbers [Bool, Bool]
    OpLess    -> zip numbers [Bool, Bool]
    OpLessEq  -> zip numbers [Bool, Bool]
    OpGreat   -> zip numbers [Bool, Bool]
    OpGreatEq -> zip numbers [Bool, Bool]
    OpBelongs -> zip numbers [Bool, Bool]
    where
        numbers = [(Int,Int), (Float,Float)]

data Unary = OpNegate | OpNot

instance Show Unary where
    show OpNegate = "Arithmetic negation"
    show OpNot    = "Logical negation"

unaryOperation :: Unary -> [(DataType, DataType)]
unaryOperation op = case op of
    OpNegate -> [(Int, Int), (Float, Float)]
    OpNot    -> [(Bool, Bool)]

--
--  Pretty printer for Statments and Expressions
--

data PrintState = PrintState { tabs :: Int } deriving (Show)

initialPState :: PrintState
initialPState = PrintState { tabs = 0 }

runPrinter :: Printer () -> String
runPrinter = (DF.foldr (++) "") . snd . runIdentity . runWriterT . (flip runStateT initialPState)

type Printer a = StateT PrintState (WriterT (Seq String) Identity) a

printStatement :: Statement -> Printer ()
printStatement st = case st of
    StNoop         -> return ()
    StAssign var e -> do
        printNonTerminal "ASSIGNMENT" 
        raiseTabs
        printNonTerminal $     "- variable: " ++ var
        printExpressionWithTag "- value: " e
        lowerTabs
    StDeclaration ds -> do
        printNonTerminal "DECLARATION"
        raiseTabs  
        printNonTerminal "DECLARATION LIST"
        lowerTabs
    StReturn e -> printExpressionWithTag "RETURN" e
    -- I/O
    StRead vs -> do
        printNonTerminal "READ"
        raiseTabs
        DF.mapM_ (printExpression . Variable) vs 
        lowerTabs
    StPrint es -> printExpressions "PRINT" es
    -- Conditional
    StIf g success fail -> do
        printNonTerminal "IF"
        raiseTabs
        printExpressionWithTag "- guard: "   g
        printStatements        "- success: " success
        printStatements        "- fail: "    fail
        lowerTabs
    StCase e cs ss -> return ()
    -- Loops
    StWhile g c -> do
        printNonTerminal "WHILE" 
        raiseTabs
        printExpressionWithTag "- guard: " g
        printStatements        "- body: "  c
        lowerTabs
    StFor var r c -> do
        printNonTerminal "FOR"
        raiseTabs
        printNonTerminal     $ "- variable: " ++ var
        printExpressionWithTag "- range: "       r
        printStatements        "- body: "        c
        lowerTabs
    StBreak    -> printNonTerminal "BREAK"
    StContinue -> printNonTerminal "CONTINUE"

--
--  Expressions printing
--
printExpression :: Expression -> Printer ()
printExpression e = case e of
    Variable  v      -> printNonTerminal $ "VARIABLE: " ++ show v
    LitInt    c      -> printNonTerminal $ "INTEGER LITERAL: " ++ show c
    LitBool   b      -> printNonTerminal $ "BOOLEAN LITERAL: " ++ show b
    LitFloat  f      -> printNonTerminal $ "FLOAT LITERAL: "   ++ show f
    LitString s      -> printNonTerminal $ "STRING LITERAL: "  ++ s
    ExpBinary op l r -> do
        printNonTerminal "BINARY OPERATION"
        raiseTabs
        printNonTerminal $ "- operator: " ++ show op
        printExpressionWithTag "- left operand:  " l
        printExpressionWithTag "- right operand: " r
        lowerTabs
    ExpUnary op e -> do
        printNonTerminal "UNARY OPERATION"
        raiseTabs
        printNonTerminal       ("- operator: " ++ show op)
        printExpressionWithTag ("- operand: "  ++ show op) e
        lowerTabs

raiseTabs :: Printer ()
raiseTabs = modify (\s -> s { tabs = tabs s + 1 })

lowerTabs :: Printer ()
lowerTabs = modify (\s -> s { tabs = tabs s - 1 })

printExpressionWithTag :: String -> Expression -> Printer ()
printExpressionWithTag tag e = do
    printNonTerminal tag
    raiseTabs >> printExpression e >> lowerTabs

printNonTerminal :: String -> Printer ()
printNonTerminal str = do
    t <- gets tabs
    tell $ DS.singleton $ replicate t '\t' ++ str ++ "\n"

printStatements :: String -> Seq Statement -> Printer ()
printStatements tag is = do
    printNonTerminal tag
    raiseTabs >> DF.mapM_ printStatement is >> lowerTabs

printExpressions :: String -> Seq Expression -> Printer ()
printExpressions tag es = do
    printNonTerminal tag
    raiseTabs >> DF.mapM_ printExpression es >> lowerTabs
