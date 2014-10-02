{-|
    Three-address code (TAC) generation module
-}
module TAC (
      TACGenerator
    , generateTAC
)
where

import           Data.Sequence    as DS (Seq, empty, length)
import           Data.Foldable    as DF (mapM_, all, and, forM_)
import           Data.Traversable (mapM)
import           Prelude          as P hiding (mapM_, mapM, all, and)

{-|
    Three-address code representation
-}

type Label = String

type Temporary = String

data Address
    = Variable  String
    | Constant  Value
    | Temporary Temporary

data Value
    = ValInt   Int
    | ValFloat Float
    | ValBool  Bool
    | ValChar  Char

data Instruction
    = Assign Operator Address Address Address -- Quadruples
    | AssignBin
    | AssignUn
    | AssignArrR
    | AssignArrL
    -- Function related instructions
    | BeginFunction Int
    | EndFunction   Int
    | PushParameter
    | PopParameters
    | Return
    | LCall
    | ACall
    -- GoTo
    | GoTo Label
    | IfZ Temporary Label
    -- Move
    | Move _ _
    -- Store
    | Store _ _ _
    -- Load
    | Load _ _ _

data Operator
    = Add   | Sub | Mul | Div | Mod
    | Equal | LessThan
    | Or    | And
    deriving (Show, Eq)

{-|
    TAC Generator Monad

    This monad needs to generate the necessary intermediate representation (IR)
    in this case the three-address code; store it temporarily as a structure for
    further processing and send it to a temporary file for debugging purposes.
-}

type TACGenerator = RWST TACReader TACWriter TACState IO a

{-|
    State
-}
TACState :: TACState
TACState = TACState
    { tempSerial  :: Int
    , labelSerial :: Int
    , table       :: SymbolTable
    , scopeId     :: ScopeNum
    }

instance Show TACState where
    show (TACState _ _) = undefined

initialState :: TACState
initialState = TACState
    { tempSerial  = 0
    , labelSerial = 0
    , table       = emptyTable
    , scopeId     = 0
    }

{-|
    Writer
 -}
type TACWriter = Seq TACInstruction

generate :: MonadWriter (Seq Instruction) m => Instruction -> m ()
generate i = tell $ singleton i

{-|
    TAC Generator private functions
 -}
getTemporary :: TACGenerator Temporary
getTemporary = do
    currentTemp <- gets tempSerial
    modify $ \s -> s { tempSerial = currentTemp + 1 }
    return $ "_T" ++ (show $ currentTemp + 1)

getLabel :: TACGenerator Label
getLabel = do
    currentLabel <- gets labelSerial
    generate $ Label currentLabel
    modify $ \s -> s { labelSerial = currentLabel + 1 }
    return $ "_L" ++ (show $ currentLabel + 1)

linearizeStatements :: StBlock -> TACGenerator ()
linearizeStatements = mapM_ linearizeStatement

linearizeStatement :: Lexeme Statement -> TACGenerator ()
linearizeStatement st = case st of
    StAssign accL expL -> do

    StReturn expL -> do

    StFunctionDef (Lex idn _) _ block -> do

    StProcedureCall idnL expLs -> do

    StRead accL -> do

    StPrint exprL -> do

    StIf expL trueBlock falseBlock -> do

    StCase expL whnLs othrBlock -> do

    StLoop befBlock expL aftBlock -> do

    StFor _ expL block -> do

    _ -> return ()
    --StNoop
    --StVariableDeclaration
    --StStructDefinition
    --StBreak
    --StContinue

linearizeExpression :: Lexeme Expression -> TacGenerator ()
linearizeExpression e = case e of
    LitInt    _ -> return Int
    LitFloat  _ -> return Float
    LitBool   _ -> return Bool
    LitChar   _ -> return Char
    LitString _ -> return String

    Variable accL -> do

    FunctionCall idnL expLs -> do
        generateLabel
        generate beginFunction
        mapM_ pushparameter
        generate endFunction

    ExpBinary (Lex op _) lExpL rExpL -> do
        case op of
        OpPlus    -> "+"
        OpMinus   -> "-"
        OpTimes   -> "*"
        OpDivide  -> "/"
        OpModulo  -> "%"
        {-OpPower   -> "^"-}
        {-OpFromTo  -> ".."-}
        OpOr      -> "or"
        OpAnd     -> "and"
        OpLess    -> "<"
        OpEqual   -> "=="
        OpUnequal -> "/="
            -- !(op1 == op2)
        OpLessEq  -> "<="
            -- (op1 == op2) || (op1 < op2)
        OpGreat   -> ">"
            -- !(op1 < op2) && !(op1 == op2)
        OpGreatEq -> ">="
             -- !(op1 < op2)
        {-OpBelongs -> "@"-}

    ExpUnary (Lex op _) -> do
        case op of
        OpNegate ->
        OpNot    ->

{-|
    TAC Pretty printer
-}
type TACPrinter = undefined

data TACPrintState = TACPrintState { tabs :: Int }

initialPrintState :: TACPrintState
initialPrintState = TACPrintState 0

type Printer a = State PrintState (WriterT (Seq String)) a

printTAC :: Instruction -> TACPrinter ()
printTAC _ = undefined
