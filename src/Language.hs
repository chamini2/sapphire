module Language where

import           Prelude
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Identity
import qualified Data.Foldable as DF (mapM_, foldr, toList)
import           Data.Sequence as DS (Seq, singleton)

type Position = (Int, Int) -- (Fila, Columna)

showPosn :: Position -> String
showPosn (line, col) = show line ++ "," ++ show col ++ ": "

----------------------------------------

data Lexeme a = Lex
    { lexInfo :: a
    , lexPosn :: Position
    } deriving (Eq)

instance Show a => Show (Lexeme a) where
    show (Lex a p) = case p of
        (0,0) -> ""
        _     -> showPosn p ++ show a

instance Functor Lexeme where
    fmap f (Lex a p) = Lex (f a) p

--------------------------------------------------------------------------------

newtype Program = Program (Seq (Lexeme Statement))

instance Show Program where
    show (Program sts) = concatMap show $ DF.toList sts

type Identifier = String

data DataType
    = Void | Int | Float | Bool | Char | String | Range | Type
    | Array (Lexeme DataType)
    | Union (Lexeme Identifier) | Record (Lexeme Identifier)
    deriving (Show, Eq)

----------------------------------------

data Statement
    -- Language
    = StNoop
    | StAssign (Lexeme Identifier) (Lexeme Expression)
    -- Definitions
    | StDeclaration (Seq (Lexeme Declaration))
    | StReturn      (Lexeme Expression)
    -- Functions
    | StFunctionDef  (Lexeme Declaration) (Seq (Lexeme DataType))
    | StFunctionImp  (Lexeme Identifier)  (Seq (Lexeme Identifier)) (Seq (Lexeme Statement))
    | StFunctionCall (Lexeme Identifier)  (Seq (Lexeme Expression))
    -- I/O
    | StRead  (Seq (Lexeme Identifier))
    | StPrint (Seq (Lexeme Expression))
    -- Conditional
    | StIf   (Lexeme Expression) (Seq (Lexeme Statement)) (Seq (Lexeme Statement))
    | StCase (Lexeme Expression) (Seq (Lexeme Case))      (Seq (Lexeme Statement))
    -- Loops
    | StWhile    (Lexeme Expression) (Seq (Lexeme Statement))
    | StFor      (Lexeme Identifier) (Lexeme Expression)  (Seq (Lexeme Statement))
    | StBreak
    | StContinue

instance Show Statement where
    show = runPrinter . printStatement

----------------------------------------

data Declaration = Declaration (Lexeme Identifier) (Lexeme DataType) Category
    deriving (Show)

data Category
    = CatVariable
    | CatFunction
    | CatParameter
    | CatRecordField
    | CatUnionField
    | CatDataType
    deriving (Eq, Show)

--data Signature = Signature (Seq (Lexeme DataType)) (Lexeme DataType)

data Case = Case (Lexeme Expression) (Seq (Lexeme Statement))
    deriving (Show)

----------------------------------------

data Expression
    -- Variable
    = Variable (Lexeme Identifier)
    -- Function call
    | FunctionCall (Lexeme Identifier) (Seq (Lexeme Expression))
    -- Literals
    | LitInt    (Lexeme Int)
    | LitFloat  (Lexeme Float)
    | LitBool   (Lexeme Bool)
    | LitChar   (Lexeme Char)
    | LitString (Lexeme String)
--    | LitRange  (Lexeme Range)
    -- Operators
    | ExpBinary (Lexeme Binary) (Lexeme Expression) (Lexeme Expression) {-DataType-}
    | ExpUnary  (Lexeme Unary)  (Lexeme Expression) {-DataType-}
--    | ExpArray  ExpressionArray

instance Show Expression where
    show = runPrinter . printExpression

data Range = FromTo (Lexeme Expression) (Lexeme Expression)
    deriving (Show)

data Binary
    = OpPlus  | OpMinus   | OpTimes | OpDivide | OpModulo | OpPower   | OpFromTo
    | OpEqual | OpUnequal | OpLess  | OpLessEq | OpGreat  | OpGreatEq | OpBelongs
    | OpOr    | OpAnd

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
    OpPower   -> zip [(Int,Int), (Float,Int)] [Int, Float]
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
    show op = case op of
        OpNegate -> "Arithmetic negation"
        OpNot    -> "Logical negation"

unaryOperation :: Unary -> [(DataType, DataType)]
unaryOperation op = case op of
    OpNegate -> [(Int, Int), (Float, Float)]
    OpNot    -> [(Bool, Bool)]

--------------------------------------------------------------------------------

--
--  Pretty printer for Statments and Expressions
--
data PrintState = PrintState { tabs :: Int } deriving (Show)

initialPState :: PrintState
initialPState = PrintState { tabs = 0 }

runPrinter :: Printer () -> String
runPrinter = DF.foldr (++) "" . snd . runIdentity . runWriterT . flip runStateT initialPState

type Printer a = StateT PrintState (WriterT (Seq String) Identity) a

----
----  Statements printing
----
printStatement :: Statement -> Printer ()
printStatement st = case st of

    StNoop         -> return ()

    StAssign (Lex var _) (Lex expr _) -> do
        printNonTerminal "ASSIGNMENT"
        raiseTabs
        printNonTerminal $ "- variable: " ++ var
        printExpressionWithTag "- value: " expr
        lowerTabs

    StDeclaration ds -> do
        printNonTerminal "DECLARATION"
        raiseTabs
        DF.mapM_ ((\(Declaration ld _ _) -> printNonTerminal (show $ lexInfo ld)) . lexInfo) ds
        lowerTabs

    StReturn (Lex expr _)     -> printExpressionWithTag "RETURN" expr

    StFunctionCall iden args -> do
        printNonTerminal "FUNCTION CALL"
        raiseTabs
        printNonTerminal "- id: "
        raiseTabs
        printNonTerminal . show $ lexInfo iden
        lowerTabs
        printNonTerminal "- arguments: "
        raiseTabs
        DF.mapM_ (printExpression . lexInfo) args
        lowerTabs
        lowerTabs

    StFunctionDef dc dts      -> do
        printNonTerminal "FUNCTION DEFINITION"
        raiseTabs
        let Declaration iden rt cat = lexInfo dc
            in do
            printNonTerminal "- id: "
            raiseTabs
            printNonTerminal . show $ lexInfo iden
            lowerTabs

            printNonTerminal "- signature: "
            raiseTabs
            DF.mapM_ (printNonTerminal . show) dts
            lowerTabs

            printNonTerminal "- return type: "
            raiseTabs
            printNonTerminal . show $ lexInfo rt
            lowerTabs
        lowerTabs

    StFunctionImp iden _ body -> do
        printNonTerminal "FUNCTION IMPLEMENTATION"
        raiseTabs
        printNonTerminal "- id: "
        raiseTabs
        printNonTerminal . show $ lexInfo iden
        lowerTabs
        printStatements "- body" body
        lowerTabs

    StRead vars -> do
        printNonTerminal "READ"
        raiseTabs
        DF.mapM_ (printExpression . Variable) vars
        lowerTabs

    StPrint exprs -> do
        printNonTerminal "PRINT"
        raiseTabs
        DF.mapM_ (printExpression . lexInfo) exprs
        lowerTabs

    StIf cond success failure -> do
        printNonTerminal "IF"
        raiseTabs
        printExpressionWithTag "- guard: " (lexInfo cond)
        printStatements "- success: " success
        printStatements "- failure: " failure
        lowerTabs

    StCase expr cases othrw -> printNonTerminal "CASE"

    StWhile cond body -> do
        printNonTerminal "WHILE"
        raiseTabs
        printExpressionWithTag "- guard: " (lexInfo cond)
        printStatements "- body: " body
        lowerTabs

    StFor var range body -> do
        printNonTerminal "FOR"
        raiseTabs
        printNonTerminal $ "- variable: " ++ lexInfo var
        printExpressionWithTag "- range: " (lexInfo range)
        printNonTerminal "- body: "
        raiseTabs >> DF.mapM_ (printStatement . lexInfo) body >> lowerTabs
        lowerTabs

    StBreak    -> printNonTerminal "BREAK"

    StContinue -> printNonTerminal "CONTINUE"

----
----  Expressions printing
----
printExpression :: Expression -> Printer ()
printExpression e = case e of
    Variable  v      -> printNonTerminal $ "VARIABLE: "        ++ show (lexInfo v)
    LitInt    c      -> printNonTerminal $ "INTEGER LITERAL: " ++ show (lexInfo c)
    LitBool   b      -> printNonTerminal $ "BOOLEAN LITERAL: " ++ show (lexInfo b)
    LitFloat  f      -> printNonTerminal $ "FLOAT LITERAL: "   ++ show (lexInfo f)
    LitString s      -> printNonTerminal $ "STRING LITERAL: "  ++ show (lexInfo s)
    ExpBinary op l r -> do
        printNonTerminal "BINARY OPERATION"
        raiseTabs
        printNonTerminal $ "- operator: " ++ show (lexInfo op)
        printExpressionWithTag "- left operand:  " (lexInfo l)
        printExpressionWithTag "- right operand: " (lexInfo r)
        lowerTabs
    ExpUnary op expr -> do
        printNonTerminal "UNARY OPERATION"
        raiseTabs
        printNonTerminal $ "- operator: " ++ show (lexInfo op)
        printExpressionWithTag "- operand: " (lexInfo expr)
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

printStatements :: String -> Seq (Lexeme Statement) -> Printer ()
printStatements tag is = do
    printNonTerminal tag
    raiseTabs >> DF.mapM_ (printStatement . lexInfo) is >> lowerTabs
