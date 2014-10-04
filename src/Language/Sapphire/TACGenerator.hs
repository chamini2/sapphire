{-|
 - TAC Generator Monad
 -
 - This monad needs to generate the necessary intermediate representation (IR)
 - in this case the three-address code; store it temporarily as a structure for
 - further processing and send it to a temporary file for debugging purposes.
 -}
module Language.Sapphire.TACGenerator
    ( TACGenerator
    , processTACGenerator
    ) where

import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad   hiding (initialWriter)
import           Language.Sapphire.SymbolTable
import           Language.Sapphire.TAC

import           Control.Monad.RWS             (RWS, execRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (mapM_)
import           Data.Sequence                 (Seq, empty, singleton)
import           Prelude                       hiding (Ordering(..), exp, mapM_)

--------------------------------------------------------------------------------

type TACGenerator = RWS TACReader TACWriter TACState

--------------------------------------------------------------------------------
-- Reader

type TACReader = ()

--------------------------------------------------------------------------------
-- State

data TACState = TACState
    { table       :: SymbolTable
    , stack       :: Stack Scope
    , scopeId     :: ScopeNum
    , ast         :: Program
    , tempSerial  :: Serial
    , labelSerial :: Serial
    }

----------------------------------------
-- Instances

instance SappState TACState where
    getTable   = table
    getStack   = stack
    getScopeId = scopeId
    getAst     = ast
    putTable   tab s = s { table   = tab }
    putStack   stk s = s { stack   = stk }
    putScopeId sc  s = s { scopeId = sc  }
    putAst     as  s = s { ast     = as  }

instance Show TACState where
    show = showSappState

----------------------------------------
-- Initial

initialState :: TACState
initialState = TACState
    { table       = emptyTable
    , stack       = topStack
    , scopeId     = topScopeNum
    , ast         = Program empty
    , tempSerial  = 0
    , labelSerial = 0
    }

--------------------------------------------------------------------------------
-- Writer

type TACWriter = Seq Instruction

----------------------------------------
-- Initial

initialWriter :: TACWriter
initialWriter = empty

----------------------------------------

generate :: Instruction -> TACGenerator ()
generate = tell . singleton

--------------------------------------------------------------------------------
-- Building the Monad

buildTACGenerator :: SymbolTable -> Program -> TACGenerator ()
buildTACGenerator tab program@(Program block) = do
    modify $ \s -> s { table = tab, ast = program }
    tell initialWriter
    linearizeStatements block

--------------------------------------------------------------------------------
-- Using the Monad

processTACGenerator :: TACReader -> SymbolTable -> Program -> TACWriter
processTACGenerator r s = generateTAC r . buildTACGenerator s

generateTAC :: TACReader -> TACGenerator a -> TACWriter
generateTAC r = snd . flip (flip execRWS r) initialState

--------------------------------------------------------------------------------
-- Monad handling

newTemporary :: TACGenerator Address
newTemporary = do
    current <- gets tempSerial
    modify $ \s -> s { tempSerial = current + 1 }
    return . Temporary $ "$T" ++ (show $ current + 1)

newLabel :: TACGenerator Label
newLabel = do
    current <- gets labelSerial
    modify $ \s -> s { labelSerial = current + 1 }
    return $ "L" ++ (show $ current + 1)

--------------------------------------------------------------------------------
-- Statements

linearizeStatements :: StBlock -> TACGenerator ()
linearizeStatements = mapM_ linearizeStatement

linearizeStatement :: Lexeme Statement -> TACGenerator ()
linearizeStatement (Lex st posn) = do
    generate . Comment $ show (row posn) ++ ", " ++ show st
    case st of
        StAssign accL expL ->  do
            expAddr <- linearizeExpression expL
            resAddr  <- newTemporary
            generate $ Move resAddr expAddr

        -- StVariableDeclaration (Lexeme Declaration)
        -- StStructDefinition    (Lexeme DataType) (Seq Field)
        -- StReturn        (Lexeme Expression)
        -- StFunctionDef   (Lexeme Identifier) Signature StBlock
        -- StProcedureCall (Lexeme Identifier) (Seq (Lexeme Expression))
        -- StRead       (Lexeme Access)
        -- StPrint      (Lexeme Expression)

        StIf expL trueBlock falseBlock -> do
            trueLabel  <- newLabel
            falseLabel <- newLabel
            endLabel   <- newLabel

            linearizeExpression expL

            generate $ PutLabel trueLabel
            linearizeStatements trueBlock
            generate $ Goto endLabel

            generate $ PutLabel falseLabel
            linearizeStatements falseBlock

            generate $ PutLabel endLabel

        -- StCase (Lexeme Expression) (Seq (Lexeme When))      StBlock
        -- StLoop     StBlock (Lexeme Expression) StBlock
        -- StFor      (Lexeme Identifier) (Lexeme Expression)  StBlock
        -- StBreak
        -- StContinue

--------------------------------------------------------------------------------
-- Expressions

linearizeExpression :: Lexeme Expression -> TACGenerator Address
linearizeExpression (Lex exp _) = case exp of

    LitInt   v -> return . Constant . ValInt   $ lexInfo v
    LitFloat v -> return . Constant . ValFloat $ lexInfo v
    LitBool  v -> return . Constant . ValBool  $ lexInfo v
    LitChar  v -> return . Constant . ValChar  $ lexInfo v

    LitString str -> undefined

    Variable accL -> undefined
        -- case acc of
        --     VariableAccess idL -> return . Variable $ lexInfo idL
        --     -- ArrayAccess    accL indexL -> do
        --         -- L.Array <- getarrayInfo
        --         -- L.type <- L.array.type.elem
        --         -- addr <- generateTemporary
        --     -- StructAccess   accL fieldL ->
        -- return $ Variable

    FunctionCall idnL expLs -> undefined
        -- here <- newLabel
        -- generate $ BeginFunction 0
        -- -- mapM_ pushParameter
        -- generate $ EndFunction 0

    ExpBinary (Lex op _) lExpL rExpL -> do
        lAddr   <- linearizeExpression lExpL
        rAddr   <- linearizeExpression rExpL
        resTemp <- newTemporary
        generate $ Assign resTemp (binaryToOperator op) lAddr rAddr
        return resTemp

    -- ExpBinary (Lex op _) lExpL rExpL -> case op of
    --     OpPlus    -> "+"
    --     OpMinus   -> "-"
    --     OpTimes   -> "*"
    --     OpDivide  -> "/"
    --     OpModulo  -> "%"
    --     {-OpPower   -> "^"-}
    --     {-OpFromTo  -> ".."-}
    --     OpOr      -> "or"
    --     OpAnd     -> "and"
    --     OpLess    -> "<"
    --     OpEqual   -> "=="
    --     OpUnequal -> "/="
    --         -- !(op1 == op2)
    --     OpLessEq  -> "<="
    --         -- (op1 == op2) || (op1 < op2)
    --     OpGreat   -> ">"
    --         -- !(op1 < op2) && !(op1 == op2)
    --     OpGreatEq -> ">="
    --          -- !(op1 < op2)
    --     {-OpBelongs -> "@"-}

    ExpUnary (Lex op _) expL -> do
        expAddr <- linearizeExpression expL
        resTemp <- newTemporary
        case op of
            OpNot    -> undefined
            OpNegate -> do
                generate $ Assign resTemp SUB (Constant $ ValInt 0) expAddr
                return resTemp

-- {-|
--     TAC Pretty printer
-- -}
-- type TACPrinter = undefined

-- type Tabs = Int

-- initialPrintState :: TACPrintState
-- initialPrintState = TACPrintState 0

-- type Printer a = State Tabs (WriterT (Seq String)) a

-- printTAC :: Instruction -> TACPrinter ()
-- printTAC _ = undefined

