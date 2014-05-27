module Language where

import           Control.Arrow          ((***))
import           Control.Monad.Identity hiding (forM_, mapM_)
import           Control.Monad.State    hiding (forM_, mapM_)
import           Control.Monad.Writer   hiding (forM_, mapM_)
import           Data.Char              (toLower)
import           Data.Foldable          as DF (foldr, forM_, mapM_, toList, concat, concatMap)
import           Data.List              (intersperse)
import           Data.Sequence          as DS (Seq, singleton, fromList)
import           Prelude                hiding (mapM_, concat, concatMap)

type Position = (Int, Int) -- (Fila, Columna)

showPosn :: Position -> String
showPosn (line, col) = show line ++ "," ++ show col

----------------------------------------

data Lexeme a = Lex
    { lexInfo :: a
    , lexPosn :: Position
    } deriving (Eq)

instance Show a => Show (Lexeme a) where
    show (Lex a p) = case p of
        (0,0) -> ""
        _     -> showPosn p ++ ": " ++ show a

instance Functor Lexeme where
    fmap f (Lex a p) = Lex (f a) p

--------------------------------------------------------------------------------

newtype Program = Program StBlock

instance Show Program where
    show (Program sts) = concatMap show $ toList sts

type Identifier = String
type StBlock = Seq (Lexeme Statement)

data Access = VariableAccess (Lexeme Identifier)
            | ArrayAccess    (Lexeme Access)     (Lexeme Expression)
            | StructAccess   (Lexeme Access)     (Lexeme Identifier)
            deriving (Eq)

getVariableAccess :: Lexeme Access -> Lexeme Identifier
getVariableAccess (Lex acc _) = case acc of
    VariableAccess idenL -> idenL
    ArrayAccess  accL _  -> getVariableAccess accL
    StructAccess accL _  -> getVariableAccess accL

instance Show Access where
    show acc = case acc of
        VariableAccess idenL       -> show (lexInfo idenL)
        ArrayAccess    accL  exprL -> show (lexInfo accL) ++ "[" ++ show (lexInfo exprL) ++ "]"
        StructAccess   accL  idenL -> show (lexInfo accL) ++ "." ++ show (lexInfo idenL)

data DataType
    = Int | Float | Bool | Char | String | Range | Type
    | Union  (Lexeme Identifier) (Seq (Lexeme Identifier, Lexeme DataType))
    | Record (Lexeme Identifier) (Seq (Lexeme Identifier, Lexeme DataType))
    | Array   (Lexeme DataType) (Lexeme Expression)
    | UserDef (Lexeme Identifier)
    | Void | Undef | TypeError  -- For compiler use
    deriving (Eq)

instance Show DataType where
    show dt = case dt of
        Int              -> "Int"
        Float            -> "Float"
        Bool             -> "Bool"
        Char             -> "Char"
        String           -> "String"
        Range            -> "Range"
        Type             -> "Type"
        (Union  iden fs) -> "Union "  ++ show (lexInfo iden) ++ " " ++ concatMap (show . (lexInfo *** lexInfo)) fs
        (Record iden fs) -> "Record " ++ show (lexInfo iden) ++ " " ++ concatMap (show . (lexInfo *** lexInfo)) fs
        (Array aDt _)    -> "[" ++ show (lexInfo aDt) ++ "]"
        (UserDef iden)   -> show $ lexInfo iden
        Void             -> "()"
        Undef            -> error "DataType Undef should never be 'shown'"
        TypeError        -> error "DataType TypeError should never be 'shown'"

----------------------------------------

data Statement
    -- Language
    = StNoop
    | StAssign (Lexeme Access) (Lexeme Expression)
    -- Definitions
    | StDeclaration      (Lexeme Declaration)
    | StDeclarationList  (DeclarationList Expression)
    | StStructDefinition (Lexeme DataType)
    -- Functions
    | StReturn       (Lexeme Expression)
    | StFunctionDef  (Lexeme Declaration) (Seq (Lexeme DataType))
    | StFunctionImp  (Lexeme Identifier)  (Seq (Lexeme Identifier)) StBlock
    | StFunctionCall (Lexeme Identifier)  (Seq (Lexeme Expression))
    -- I/O
    | StRead  (Seq (Lexeme Access))
    | StPrint (Seq (Lexeme Expression))
    -- Conditional
    | StIf   (Lexeme Expression) StBlock StBlock
    | StCase (Lexeme Expression) (Seq (Lexeme When))      StBlock
    -- Loops
    | StLoop     StBlock (Lexeme Expression) StBlock
    | StFor      (Lexeme Identifier) (Lexeme Expression)  StBlock
    | StBreak
    | StContinue

instance Show Statement where
    show = runPrinter . printStatement

----------------------------------------

data Declaration = Declaration (Lexeme Identifier) (Lexeme DataType) Category
    deriving (Show)

type DeclarationList a = Seq (Lexeme Declaration, Maybe (Lexeme a))

data Category
    = CatVariable
    | CatFunction
    | CatParameter
    | CatRecordField
    | CatUnionField
    | CatUserDef
    deriving (Eq)

instance Show Category where
    show CatVariable    = "variable"
    show CatFunction    = "function"
    show CatParameter   = "parameter"
    show CatRecordField = "record field"
    show CatUnionField  = "union field"
    show CatUserDef     = "data type"

data When = When (Seq (Lexeme Expression)) StBlock
    deriving (Show)

----------------------------------------

data Expression
    -- Variable
    = Variable (Lexeme Access)
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
    deriving (Eq)

instance Show Expression where
    show = runPrinter . printExpression

data Range = FromTo (Lexeme Expression) (Lexeme Expression)
    deriving (Show)

data Binary
    = OpPlus  | OpMinus   | OpTimes | OpDivide | OpModulo | OpPower   | OpFromTo
    | OpEqual | OpUnequal | OpLess  | OpLessEq | OpGreat  | OpGreatEq | OpBelongs
    | OpOr    | OpAnd
    deriving (Eq)

instance Show Binary where
    show op = case op of
        OpPlus    -> "arithmetic addition"
        OpMinus   -> "arithmetic substraction"
        OpTimes   -> "arithmetic multiplication"
        OpDivide  -> "arithmetic division"
        OpModulo  -> "arithmetic Modulo"
        OpPower   -> "arithmetic power"
        OpFromTo  -> "range construction operator"
        OpOr      -> "logical disjunction"
        OpAnd     -> "logical conjunction"
        OpEqual   -> "equal to"
        OpUnequal -> "not equal to"
        OpLess    -> "less than"
        OpLessEq  -> "less than or equal to"
        OpGreat   -> "greater than"
        OpGreatEq -> "greater than or equal to"
        OpBelongs -> "belongs to Range"

binaryOperation :: Binary -> Seq ((DataType, DataType), DataType)
binaryOperation op = fromList $ case op of
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
    OpBelongs -> zip [(Int,Range), (Float,Range)] [Bool, Bool]
    where
        numbers = [(Int,Int), (Float,Float)]

data Unary = OpNegate | OpNot
    deriving (Eq)

instance Show Unary where
    show op = case op of
        OpNegate -> "Arithmetic negation"
        OpNot    -> "Logical negation"

unaryOperation :: Unary -> Seq (DataType, DataType)
unaryOperation op = fromList $ case op of
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
runPrinter = concat . snd . runIdentity . runWriterT . flip runStateT initialPState

type Printer a = StateT PrintState (WriterT (Seq String) Identity) a

----
----  Statements printing
----
printStatement :: Statement -> Printer ()
printStatement st = case st of
    StNoop         -> return ()

    StAssign var (Lex expr _) -> do
        printNonTerminal "ASSIGNMENT"
        raiseTabs
        printNonTerminal $ "- variable: " ++ show var
        printExpressionWithTag "- value: " expr
        lowerTabs

    StDeclaration (Lex (Declaration ld dt _) _) -> do
        printNonTerminal "DECLARATION"
        raiseTabs
        printNonTerminal $ show (lexInfo dt) ++ " " ++ lexInfo ld
        lowerTabs

    StStructDefinition (Lex dt _) -> do
        let (typeStr, fields, iden) = case dt  of
                Record iden fields -> ("Record", fields, iden)
                Union  iden fields -> ("Union" , fields, iden)
        printNonTerminal (typeStr ++ show dt ++ " " ++ lexInfo iden)
        raiseTabs
        forM_ fields $ \(Lex fIden _, Lex fDt _) ->
            printNonTerminal $ "- field: " ++ fIden ++ " :: " ++ show fDt
        lowerTabs

    StReturn (Lex expr _)     -> printExpressionWithTag "RETURN" expr

    StFunctionCall iden args -> do
        printNonTerminal "FUNCTION CALL"
        raiseTabs
        printNonTerminal "- function name: "
        raiseTabs
        printNonTerminal . show $ lexInfo iden
        lowerTabs
        printNonTerminal "- arguments: "
        raiseTabs
        mapM_ (printExpression . lexInfo) args
        lowerTabs
        lowerTabs

    StFunctionDef dc dts      -> do
        printNonTerminal "FUNCTION DEFINITION"
        raiseTabs
        let Declaration iden rt _ = lexInfo dc
            in do
            printNonTerminal "- function name: "
            raiseTabs
            printNonTerminal . show $ lexInfo iden
            lowerTabs

            printNonTerminal "- signature: "
            raiseTabs
            let newDts = map (show . lexInfo) $ toList dts
            printNonTerminal . concat $ intersperse ", " newDts
            lowerTabs

            printNonTerminal "- return type: "
            raiseTabs
            printNonTerminal . show $ lexInfo rt
            lowerTabs
        lowerTabs

    StFunctionImp iden _ body -> do
        printNonTerminal "FUNCTION IMPLEMENTATION"
        raiseTabs
        printNonTerminal "- function name: "
        raiseTabs
        printNonTerminal . show $ lexInfo iden
        lowerTabs
        printStatements "- body" body
        lowerTabs

    StRead vars -> do
        printNonTerminal "READ"
        raiseTabs
        mapM_ (printExpression . Variable) vars
        lowerTabs

    StPrint exprs -> do
        printNonTerminal "PRINT"
        raiseTabs
        mapM_ (printExpression . lexInfo) exprs
        lowerTabs

    StIf cond success failure -> do
        printNonTerminal "IF"
        raiseTabs
        printExpressionWithTag "- guard: " (lexInfo cond)
        printStatements "- success: " success
        printStatements "- failure: " failure
        lowerTabs

    StCase expr cases othrw -> do
        printNonTerminal "CASE"
        raiseTabs
        printExpressionWithTag "- expression: " $ lexInfo expr
        forM_ cases $ \(Lex (When exps body) _) -> do
            printNonTerminal "- when: "
            raiseTabs
            mapM_ (printExpression . lexInfo) exps
            printStatements "- body: " body
            lowerTabs
        printStatements "- otherwise: " othrw
        lowerTabs

    StLoop rep cond body -> do
        printNonTerminal "LOOP"
        raiseTabs
        printStatements "- repeat: " rep
        printExpressionWithTag "- guard: " (lexInfo cond)
        printStatements "- body: " body
        lowerTabs

    StFor var range body -> do
        printNonTerminal "FOR"
        raiseTabs
        printNonTerminal $ "- variable: " ++ lexInfo var
        printExpressionWithTag "- range: " (lexInfo range)
        printNonTerminal "- body: "
        raiseTabs >> mapM_ (printStatement . lexInfo) body >> lowerTabs
        lowerTabs

    StBreak    -> printNonTerminal "BREAK"

    StContinue -> printNonTerminal "CONTINUE"

----
----  Expressions printing
----
printExpression :: Expression -> Printer ()
printExpression e = case e of
    Variable var -> printNonTerminal $ "VARIABLE: " ++ show var
    FunctionCall iden args -> do
        printNonTerminal "FUNCTION CALL"
        raiseTabs
        printNonTerminal $ "- function name: " ++ show (lexInfo iden)
        printNonTerminal "- arguments: "
        raiseTabs
        mapM_ (printExpression . lexInfo) args
        lowerTabs
        lowerTabs
    LitInt    i -> printNonTerminal $ "INTEGER LITERAL: "   ++ show (lexInfo i)
    LitChar   c -> printNonTerminal $ "CHARACTER LITERAL: " ++ show (lexInfo c)
    LitBool   b -> printNonTerminal $ "BOOLEAN LITERAL: "   ++ map toLower (show (lexInfo b))
    LitFloat  f -> printNonTerminal $ "FLOAT LITERAL: "     ++ show (lexInfo f)
    LitString s -> printNonTerminal $ "STRING LITERAL: "    ++ show (lexInfo s)
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

printStatements :: String -> StBlock -> Printer ()
printStatements tag is = do
    printNonTerminal tag
    raiseTabs >> mapM_ (printStatement . lexInfo) is >> lowerTabs
