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

-- import           Control.Arrow                 ((&&&))
import           Control.Monad                 (liftM, void, unless)
import           Control.Monad.RWS             (RWS, execRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (mapM_, forM_)
import           Data.Maybe                    (fromJust)
import           Data.Sequence                 (Seq, empty, singleton, length, null, zip)
import           Data.Traversable              (mapM, forM)
import           Prelude                       hiding (Ordering(..), exp, mapM, mapM_, length, null, zip)

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
linearizeStatements = mapM_ $ \stL -> do
    nextLabel <- newLabel
    linearizeStatement nextLabel stL
    generate $ PutLabel nextLabel $ "next statement of the one in line " ++ show (row $ lexPosn stL)

linearizeStatement :: Label -> Lexeme Statement -> TACGenerator ()
linearizeStatement nextLabel (Lex st posn) = do
    generate . Comment $ "line " ++ show (row posn) ++ ", " ++ show st
    case st of

        StAssign accL expL ->  do

            (resAddr, accDt) <- getAccessAddress accL

            -- When assigning a Boolean expression, use jumping code
            if accDt == Bool then do
                trueLabel  <- newLabel
                falseLabel <- newLabel
                jumpingCode expL trueLabel falseLabel

                generate $ PutLabel trueLabel "true label for assignment"
                generate $ Assign resAddr (Constant $ ValBool True)
                generate $ Goto nextLabel

                generate $ PutLabel falseLabel "false label for assignment"
                generate $ Assign resAddr (Constant $ ValBool False)

            else do
                expAddr <- linearizeExpression expL
                generate $ Assign resAddr expAddr

        -- StVariableDeclaration

        StStructDefinition _ _ -> enterScope >> exitScope      -- For scopeStack maintenance

        -- StReturn        (Lexeme Expression)
        -- StFunctionDef   (Lexeme Identifier) Signature StBlock

        StProcedureCall idnL prmLs -> do
            prmAddrs <- mapM linearizeExpression prmLs
            mapM_ (generate . PushParameter) prmAddrs
            generate $ PCall (lexInfo idnL) (length prmAddrs)

        -- StRead       (Lexeme Access)
        -- StPrint      (Lexeme Expression)

        StIf expL trueBlock falseBlock -> do
            trueLabel  <- newLabel
            falseLabel <- newLabel
            endLabel   <- newLabel

            jumpingCode expL trueLabel falseLabel

            generate $ PutLabel trueLabel "then label for `if`"
            linearizeStatements trueBlock
            generate $ Goto endLabel

            generate $ PutLabel falseLabel "else label for `if`"
            linearizeStatements falseBlock

            generate $ PutLabel endLabel "end of `if`"

        StCase expL whnLs othrBlock -> do
            testLabel <- newLabel
            othrLabel <- newLabel

            expAddr <- linearizeExpression expL
            generate $ Goto testLabel

            whenLabels <- forM whnLs $ \(Lex (When _ whnBlock) whnPosn) -> do
                whnLabel <- newLabel
                generate $ PutLabel whnLabel $ "when label for `case` in line " ++ show (row whnPosn)

                linearizeStatements whnBlock
                generate $ Goto nextLabel

                return whnLabel

            -- Only when there is an `otherwise`
            unless (null othrBlock) $ do
                generate $ PutLabel othrLabel "otherwise label for `case`"
                linearizeStatements othrBlock
                generate $ Goto nextLabel

            generate $ PutLabel testLabel "test label for `case`"

            -- For every when, with the block's label
            forM_ (zip whenLabels whnLs) $ \(whnLabel, Lex (When whnExpLs _) _) ->
                forM_ whnExpLs $ \whnExpL -> do
                    whnAddr <- linearizeExpression whnExpL
                    generate $ IfGoto expAddr EQ whnAddr whnLabel

            -- Only jump when there is an `otherwise`
            unless (null othrBlock) . generate $ Goto othrLabel

        StLoop befBlock expL aftBlock -> do
            befLabel <- newLabel
            aftLabel <- newLabel

            generate $ PutLabel befLabel "before block for `loop`"
            linearizeStatements befBlock

            jumpingCode expL aftLabel nextLabel

            generate $ PutLabel aftLabel "after block for `loop`"
            linearizeStatements aftBlock

            generate $ Goto befLabel

        -- StFor      (Lexeme Identifier) (Lexeme Expression)  StBlock
        -- StBreak
        -- StContinue

        _ -> return ()

--------------------------------------------------------------------------------
-- Expressions

jumpingCode :: Lexeme Expression -> Label -> Label -> TACGenerator ()
jumpingCode expL@(Lex exp _) trueLabel falseLabel = case exp of

    LitBool v -> do
        generate . Goto $ if lexInfo v then trueLabel else falseLabel

    ExpBinary (Lex op _) lExpL rExpL -> case op of
        OpOr  -> do
            rightLabel <- newLabel
            jumpingCode lExpL trueLabel rightLabel
            generate $ PutLabel rightLabel "right operand of `or`"
            jumpingCode rExpL trueLabel falseLabel
        OpAnd -> do
            rightLabel <- newLabel
            jumpingCode lExpL rightLabel falseLabel
            generate $ PutLabel rightLabel "right operand of `and`"
            jumpingCode rExpL trueLabel falseLabel
        _ -> if comp op then do
                lAddr <- linearizeExpression lExpL
                rAddr <- linearizeExpression rExpL
                generate $ IfGoto lAddr (binaryToRelation op) rAddr trueLabel
                generate $ Goto falseLabel
            else void $ linearizeExpression expL
        where
            comp = flip elem [OpEqual,OpUnequal,OpLess,OpLessEq,OpGreat,OpGreatEq]

    ExpUnary (Lex op _) unExpL -> case op of
        OpNot -> jumpingCode unExpL falseLabel trueLabel
        _     -> void $ linearizeExpression expL

    _ -> error "TACGenerator.jumpingCode: should not get jumping code for non-boolean expressions"


linearizeExpression :: Lexeme Expression -> TACGenerator Address
linearizeExpression (Lex exp _) = case exp of

    LitInt   v -> return . Constant . ValInt   $ lexInfo v
    LitFloat v -> return . Constant . ValFloat $ lexInfo v
    LitBool  v -> return . Constant . ValBool  $ lexInfo v
    LitChar  v -> return . Constant . ValChar  $ lexInfo v

    LitString str -> newTemporary

    Variable accL -> newTemporary
        -- case acc of
        --     VariableAccess idL -> return . Variable $ lexInfo idL
        --     -- ArrayAccess    accL indexL -> do
        --         -- L.Array <- getarrayInfo
        --         -- L.type <- L.array.type.elem
        --         -- addr <- generateTemporary
        --     -- StructAccess   accL fieldL ->
        -- return $ Variable

    FunctionCall idnL prmLs -> do
        resTemp  <- newTemporary
        prmAddrs <- mapM linearizeExpression prmLs
        mapM_ (generate . PushParameter) prmAddrs
        generate $ FCall resTemp (lexInfo idnL) (length prmAddrs)
        return resTemp

    ExpBinary (Lex op _) lExpL rExpL -> do
        lAddr   <- linearizeExpression lExpL
        rAddr   <- linearizeExpression rExpL
        resTemp <- newTemporary
        generate $ AssignBin resTemp lAddr (binaryToBinOperator op) rAddr
        return resTemp

    ExpUnary (Lex op _) expL -> do
        expAddr <- linearizeExpression expL
        resTemp <- newTemporary
        generate $ AssignUn resTemp (unaryToUnOperator op) expAddr
        return resTemp

--------------------------------------------------------------------------------

getAccessAddress :: Lexeme Access -> TACGenerator (Address, DataType)
getAccessAddress accL = do
    -- It works only for Variable for now
    let VariableAccess idnL = lexInfo accL
        idn                 = lexInfo idnL
    dt <- liftM (fromJust) $ getsSymbol idn (lexInfo . dataType)
    return (Name idn, dt)

-- getAccessAddress :: Lexeme Access -> TACGenerator Address
-- getAccessAddress accL = do
--     let deepAccZpp              = deepAccess $ focusAccess accL
--         VariableAccess deepIdnL = lexInfo $ defocusAccess deepAccZpp
--         deepIdn                 = lexInfo deepIdnL

--     maySymI <- getsSymbol deepIdn (offset &&& dataType)
--     let (off, dtL) = fromJust maySymI

--     let deepDtZpp          = deepDataType $ focusDataType dtL
--         (deepDtL, deepDim) = defocusDataType deepDtZpp

--     innerWdt <- getDataTypeWidth dtL

--     wdt <- calculateRelativeOffset deepAccZpp deepDtZpp (Constant $ ValInt innerWdt) (Constant $ ValInt 0)
--     return . Constant $ ValInt off

----------------------------------------

-- getDataTypeWidth :: Lexeme DataType -> TACGenerator Width
-- getDataTypeWidth dtL = do
--     let deepDt = lexInfo . fst . defocusDataType . deepDataType $ focusDataType dtL
--     liftM fromJust $ getsSymbol (toIdentifier deepDt) width

-- calculateRelativeOffset :: AccessZipper -> DataTypeZipper -> Address -> Address -> TACGenerator Address
-- calculateRelativeOffset accZ dtZ wdt off = case defocusAccess `fmap` backAccess accZ of

--     Just accL -> case lexInfo accL of

--         ArrayAccess _ expL -> do

--             -- Calculate the index's value
--             expAddr <- linearizeExpression expL

--             mulTemp <- newTemporary
--             addTemp <- newTemporary
--             generate $ AssignBin mulTemp MUL expAddr wdt
--             generate $ AssignBin addTemp ADD mulTemp off

--             calculateRelativeOffset (fromJust $ backAccess accZ) dtZ wdt off

--         StructAccess _ fldIdnL -> do

--             -- This (fromJust . fromJust) may be dangerous (or not)
--             -- tab <- liftM (fromJust . fromJust) $ getsSymbol (toIdentifier dt) fields

--             -- -- Looks for the field in the struct's SymbolTable
--             -- let mayFldDt = (lexInfo . dataType) <$> lookup (lexInfo fldIdnL) tab
--             -- constructDataType (fromJust $ backAccess accZ) (fromJust mayFldDt)
--             return off

--     Nothing -> return off

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

