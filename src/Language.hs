module Language where

import           Control.Arrow          ((***))
import           Control.Monad.Identity hiding (forM_, mapM_)
import           Control.Monad.State    hiding (forM_, mapM_)
import           Control.Monad.Writer   hiding (forM_, mapM_)
import           Data.Char              (toLower)
import           Data.Foldable          as DF (foldr, forM_, mapM_, toList, concat, concatMap)
import           Data.Functor           ((<$))
import           Data.Maybe             (fromJust)
import           Data.List              (intercalate)
import           Data.Sequence          as DS (Seq, singleton, fromList)
import           Prelude                hiding (mapM_, concat, concatMap)

type Position = (Int, Int) -- (Row, Column)

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

----------------------------------------

data Access = VariableAccess (Lexeme Identifier)
            | ArrayAccess    (Lexeme Access)     (Lexeme Expression)
            | StructAccess   (Lexeme Access)     (Lexeme Identifier)
            deriving (Eq)

instance Show Access where
    show acc = case acc of
        VariableAccess idenL       -> lexInfo idenL
        ArrayAccess    accL indexL -> show (lexInfo accL) ++ "[" ++ showIndex (lexInfo indexL) ++ "]"
        StructAccess   accL fieldL -> show (lexInfo accL) ++ "." ++ lexInfo fieldL

{-
 - deriving the AccessHistory type
 -
 - acc = (var x iden) + (arr x acc x expr) + (str x acc x iden)
 - expr = A
 - iden = B
 -
 - acc = (1 x B) + (1 x acc x A) + (1 x acc x B)
 - acc = (1 x B) + (acc x A)     + (acc x B)
 -
 - acc' = A + B
 -}

data AccessHistory = HistoryArray  (Lexeme Expression)
                   | HistoryStruct (Lexeme Identifier)
                   deriving (Show)

type Thread = [Lexeme AccessHistory]

type Zipper = (Lexeme Access, Thread)

----------------------------------------

focusAccess :: Lexeme Access -> Zipper
focusAccess accL = (accL, [])

defocusAccess :: Zipper -> Lexeme Access
defocusAccess (accL, _) = accL

inArrayAccess :: Zipper -> Maybe Zipper
inArrayAccess (histL@(Lex acc _), ths) = case acc of
    ArrayAccess accL indexL -> Just (accL, (HistoryArray indexL <$ histL) : ths)
    _                       -> Nothing

inStructAccess :: Zipper -> Maybe Zipper
inStructAccess (histL@(Lex acc _), ths) = case acc of
    StructAccess accL fieldL -> Just (accL, (HistoryStruct fieldL <$ histL) : ths)
    _                        -> Nothing

inAccess :: Zipper -> Maybe Zipper
inAccess z@(Lex acc _, ths) = case acc of
    VariableAccess idenL       -> Nothing
    ArrayAccess    accL indexL -> inArrayAccess z
    StructAccess   accL fieldL -> inStructAccess z

backAccess :: Zipper -> Maybe Zipper
backAccess (accL, ths) = case ths of
    []                      -> Nothing
    histL@(Lex hist _) : ts -> case hist of
        HistoryArray  indexL -> Just (ArrayAccess  accL indexL <$ histL, ts)
        HistoryStruct fieldL -> Just (StructAccess accL fieldL <$ histL, ts)

topAccess :: Zipper -> Zipper
topAccess z@(accL, ths) = case ths of
    []     -> z
    t : ts -> topAccess $ fromJust $ backAccess z

deepAccess :: Zipper -> Zipper
deepAccess z@(Lex acc _, ths) = case acc of
    VariableAccess _ -> z
    _                -> deepAccess $ fromJust $ inAccess z

----------------------------------------

data DataType
    = Int | Float | Bool | Char | String | Range | Type
    | Union  (Lexeme Identifier) (Seq Field)
    | Record (Lexeme Identifier) (Seq Field)
    | Array   (Lexeme DataType) (Lexeme Expression)
    | UserDef (Lexeme Identifier)
    | Void | Undef | TypeError  -- For compiler use
    deriving (Eq)

instance Show DataType where
    show dt = case dt of
        Int            -> "Int"
        Float          -> "Float"
        Bool           -> "Bool"
        Char           -> "Char"
        String         -> "String"
        Range          -> "Range"
        Type           -> "Type"
        Union  iden fs -> "Union " ++ lexInfo iden
        Record iden fs -> "Record " ++ lexInfo iden
        Array aDtL _   -> "[" ++ show (lexInfo aDtL) ++ "]"
        UserDef idenL  -> lexInfo idenL
        Void           -> "()"
        Undef          -> error "DataType Undef should never be 'shown'"
        TypeError      -> error "DataType TypeError should never be 'shown'"

type Field = (Lexeme Identifier, Lexeme DataType)

getFields :: DataType -> Seq Field
getFields dt = case dt of
    Record _ fields -> fields
    Union  _ fields -> fields
    _               -> error "Language.getFields: should not attempt to get fields from non user-defined DataType"

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
    | CatField
    | CatUserDef
    deriving (Eq)

instance Show Category where
    show CatVariable  = "variable"
    show CatFunction  = "function"
    show CatParameter = "parameter"
    show CatField     = "field"
    show CatUserDef   = "data type"

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

    StAssign accL exprL -> do
        printNonTerminal "ASSIGNMENT"
        raiseTabs
        printNonTerminal $ "- variable: " ++ show (lexInfo accL)
        printExpressionWithTag "- value: " (lexInfo exprL)
        lowerTabs

    StDeclaration (Lex (Declaration ld dt _) _) -> do
        printNonTerminal "DECLARATION"
        raiseTabs
        printNonTerminal $ showDataType (lexInfo dt) ++ " " ++ lexInfo ld
        lowerTabs

    StStructDefinition dtL -> do
        let fields = getFields (lexInfo dtL)
        printNonTerminal $ show (lexInfo dtL)
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
            printNonTerminal $ intercalate ", " newDts
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

    StRead accLs -> do
        printNonTerminal "READ"
        raiseTabs
        mapM_ (printExpression . Variable) accLs
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
    Variable accL -> printNonTerminal $ "VARIABLE: " ++ show (lexInfo accL)
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

--
--  Literal printing
--
showIndex :: Expression -> String
showIndex = runPrinter . printExpressionIndex

printExpressionIndex :: Expression -> Printer ()
printExpressionIndex e = case e of
    Variable accL -> printNonTerminalIndex $ show (lexInfo accL)
    FunctionCall iden args -> do
        printNonTerminalIndex (lexInfo iden ++ "(")
        unless (null $ toList args) $ do
            printExpressionIndex . lexInfo . head $ toList args
            mapM_ func . tail $ toList args
        printNonTerminalIndex ")"
        where
            func argL = printNonTerminalIndex ", " >> printExpressionIndex (lexInfo argL)
    LitInt    i -> printNonTerminalIndex $ show (lexInfo i)
    LitChar   c -> printNonTerminalIndex $ show (lexInfo c)
    LitBool   b -> printNonTerminalIndex $ show (lexInfo b)
    LitFloat  f -> printNonTerminalIndex $ show (lexInfo f)
    LitString s -> printNonTerminalIndex $ show (lexInfo s)
    ExpBinary op l r -> do
        printExpressionIndex (lexInfo l)
        printNonTerminalIndex " "
        printBinary (lexInfo op)
        printNonTerminalIndex " "
        printExpressionIndex (lexInfo r)
    ExpUnary op expr -> do
        printUnary (lexInfo op)
        printExpressionIndex (lexInfo expr)

printBinary :: Binary -> Printer ()
printBinary op = printNonTerminalIndex $ case op of
    OpPlus    -> "+"
    OpMinus   -> "-"
    OpTimes   -> "*"
    OpDivide  -> "/"
    OpModulo  -> "%"
    OpPower   -> "^"
    OpFromTo  -> ".."
    OpOr      -> "||"
    OpAnd     -> "&&"
    OpEqual   -> "=="
    OpUnequal -> "/="
    OpLess    -> "<"
    OpLessEq  -> "<="
    OpGreat   -> ">"
    OpGreatEq -> ">="
    OpBelongs -> "@"

printUnary :: Unary -> Printer ()
printUnary op = printNonTerminalIndex $ case op of
    OpNegate -> "-"
    OpNot    -> "not"

printNonTerminalIndex :: String -> Printer ()
printNonTerminalIndex = tell . DS.singleton

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

showDataType :: DataType -> String
showDataType dt = case dt of
    Array aDtL indexL -> showDataType (lexInfo aDtL) ++ "[" ++ showIndex (lexInfo indexL) ++ "]"
    _                 -> show dt
