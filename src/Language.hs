{-# LANGUAGE GADTs #-}
module Language where

import           Prelude
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Identity
import qualified Data.Foldable as DF (mapM_, foldr)
import           Data.Sequence as DS (Seq, singleton)

type Position = (Int, Int) -- (Fila, Columna)

showPosn :: Position -> String
showPosn (line, col) = "line " ++ show line ++ ", column " ++ show col ++ ": "

----------------------------------------

data Lexeme a = Lex a Position
    deriving (Eq)

instance (Show a) => Show (Lexeme a) where
    show (Lex a p) = showPosn p ++ show a

--------------------------------------------------------------------------------

--newtype Program = StFunction
--    deriving (Show)
--newtype Program = Program [Statement]
--    deriving (Show)
newtype Program = Program (Seq (Lexeme Statement))
    deriving (Show)

type Identifier = String

data DataType = Void | Int | Float | Bool | Char | String | Range | Type-- | Array
    deriving (Show, Eq)

----------------------------------------

data Statement where
    -- Language
    StNoop   :: Statement
    StAssign :: Lexeme Identifier -> Lexeme Expression -> Statement
    -- Definitions
    StDeclaration :: Seq (Lexeme Declaration) -> Statement
    StReturn      :: Lexeme Expression    -> Statement
    -- I/O
    StRead  :: Seq (Lexeme Identifier) -> Statement
    StPrint :: Seq (Lexeme Expression) -> Statement
    -- Conditional
    StIf   :: Lexeme Expression -> Seq (Lexeme Statement) -> Seq (Lexeme Statement) -> Statement
    StCase :: Lexeme Expression -> Seq (Lexeme Case)      -> Seq (Lexeme Statement) -> Statement
    -- Loops
    StWhile    :: Lexeme Expression -> Seq (Lexeme Statement) ->  Statement
    StFor      :: Lexeme Identifier -> Lexeme Expression  -> Seq (Lexeme Statement) -> Statement
    StBreak    :: Statement
    StContinue :: Statement

instance Show Statement where
    show = runPrinter . printStatement

----------------------------------------

data Declaration where
    Declaration :: Lexeme Identifier -> Lexeme DataType -> Category -> Declaration
    deriving (Show)

data Category = CatVariable
              | CatFunction
              | CatParameter
              | CatRecordField
              | CatUnionField
              | CatDataType
              deriving (Eq, Show)

data Case = Case (Lexeme Expression) (Seq (Lexeme Statement))
    deriving (Show)

----------------------------------------

data Expression where
    -- Variable
    Variable :: Lexeme Identifier -> Expression
    -- Literals
    LitInt    :: Lexeme Int    -> Expression
    LitFloat  :: Lexeme Float  -> Expression
    LitBool   :: Lexeme Bool   -> Expression
    LitChar   :: Lexeme Char   -> Expression
    LitString :: Lexeme String -> Expression
    --LitRange  :: Lexeme Range  -> Expression
    -- Operators
    ExpBinary :: Lexeme Binary   -> Lexeme Expression -> Lexeme Expression -> {-DataType ->-} Expression
    ExpUnary  :: Lexeme Unary    -> Lexeme Expression -> {-DataType   ->-} Expression
    --ExpArray  :: ExpressionArray
    deriving (Show)

data Range where
    FromTo :: Lexeme Expression -> Lexeme Expression -> Range
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
runPrinter = DF.foldr (++) "" . snd . runIdentity . runWriterT . flip runStateT initialPState

type Printer a = StateT PrintState (WriterT (Seq String) Identity) a

printStatement :: Statement -> Printer ()
printStatement st = case st of
    StNoop         -> return ()
--    StAssign var e -> do
--        printNonTerminal "ASSIGNMENT"
--        raiseTabs
--        printNonTerminal $     "- variable: " ++ var
--        printExpressionWithTag "- value: " e
--        lowerTabs
--    StDeclaration ds -> do
--        printNonTerminal "DECLARATION"
--        raiseTabs
--        printNonTerminal "DECLARATION LIST"
--        lowerTabs
--    StReturn e -> printExpressionWithTag "RETURN" e
--    -- I/O
--    StRead vs -> do
--        printNonTerminal "READ"
--        raiseTabs
--        DF.mapM_ (printExpression . Variable) vs
--        lowerTabs
--    StPrint es -> printExpressions "PRINT" es
--    -- Conditional
--    StIf g success fail -> do
--        printNonTerminal "IF"
--        raiseTabs
--        printExpressionWithTag "- guard: "   g
--        printStatements        "- success: " success
--        printStatements        "- fail: "    fail
--        lowerTabs
--    StCase e cs ss -> return ()
--    -- Loops
--    StWhile g c -> do
--        printNonTerminal "WHILE"
--        raiseTabs
--        printExpressionWithTag "- guard: " g
--        printStatements        "- body: "  c
--        lowerTabs
--    StFor var r c -> do
--        printNonTerminal "FOR"
--        raiseTabs
--        printNonTerminal     $ "- variable: " ++ var
--        printExpressionWithTag "- range: "       r
--        printStatements        "- body: "        c
--        lowerTabs
--    StBreak    -> printNonTerminal "BREAK"
--    StContinue -> printNonTerminal "CONTINUE"

----
----  Expressions printing
----
--printExpression :: Expression -> Printer ()
--printExpression e = case e of
--    Variable  v      -> printNonTerminal $ "VARIABLE: " ++ show v
--    LitInt    c      -> printNonTerminal $ "INTEGER LITERAL: " ++ show c
--    LitBool   b      -> printNonTerminal $ "BOOLEAN LITERAL: " ++ show b
--    LitFloat  f      -> printNonTerminal $ "FLOAT LITERAL: "   ++ show f
--    LitString s      -> printNonTerminal $ "STRING LITERAL: "  ++ s
--    ExpBinary op l r -> do
--        printNonTerminal "BINARY OPERATION"
--        raiseTabs
--        printNonTerminal $ "- operator: " ++ show op
--        printExpressionWithTag "- left operand:  " l
--        printExpressionWithTag "- right operand: " r
--        lowerTabs
--    ExpUnary op e -> do
--        printNonTerminal "UNARY OPERATION"
--        raiseTabs
--        printNonTerminal       ("- operator: " ++ show op)
--        printExpressionWithTag ("- operand: "  ++ show op) e
--        lowerTabs

--raiseTabs :: Printer ()
--raiseTabs = modify (\s -> s { tabs = tabs s + 1 })

--lowerTabs :: Printer ()
--lowerTabs = modify (\s -> s { tabs = tabs s - 1 })

--printExpressionWithTag :: String -> Expression -> Printer ()
--printExpressionWithTag tag e = do
--    printNonTerminal tag
--    raiseTabs >> printExpression e >> lowerTabs

--printNonTerminal :: String -> Printer ()
--printNonTerminal str = do
--    t <- gets tabs
--    tell $ DS.singleton $ replicate t '\t' ++ str ++ "\n"

--printStatements :: String -> Seq Statement -> Printer ()
--printStatements tag is = do
--    printNonTerminal tag
--    raiseTabs >> DF.mapM_ printStatement is >> lowerTabs

--printExpressions :: String -> Seq Expression -> Printer ()
--printExpressions tag es = do
--    printNonTerminal tag
--    raiseTabs >> DF.mapM_ printExpression es >> lowerTabs
