{-# LANGUAGE TupleSections #-}
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
import           Language.Sapphire.TypeChecker (processExpressionChecker, processAccessChecker)

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (liftM, unless, void)
import           Control.Monad.RWS             (RWS, execRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (forM_, mapM_)
import           Data.Maybe                    (fromJust)
import           Data.Sequence                 (Seq, empty, length, null,
                                                reverse, singleton, zip)
import           Data.Traversable              (forM, mapM)
import           Prelude                       hiding (Ordering (..), exp,
                                                length, mapM, mapM_, null,
                                                reverse, zip, lookup)

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
    , loopStack   :: Stack (Label, Label)   -- (StartLoopLabel, EndLoopLabel)
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
    , loopStack   = emptyStack
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

enterLoop :: Label -> Label -> TACGenerator ()
enterLoop startLoop endLoop = modify $ \s -> s { loopStack = push (startLoop, endLoop) (loopStack s) }

exitLoop :: TACGenerator ()
exitLoop = modify $ \s -> s { loopStack = pop (loopStack s) }

currentLoop :: TACGenerator (Label, Label)
currentLoop = gets (top . loopStack)

----------------------------------------

newTemporary :: TACGenerator Address
newTemporary = do
    current <- liftM succ $ gets tempSerial
    modify $ \s -> s { tempSerial = current }
    return . Temporary $ "$T" ++ show current

newLabel :: TACGenerator Label
newLabel = do
    current <- liftM succ $ gets labelSerial
    modify $ \s -> s { labelSerial = current}
    return $ "L" ++ show current

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
            tab <- gets table
            let expDt = processExpressionChecker tab expL
            accAddr <- getAccessAddress accL

            -- When assigning a Boolean expression, use jumping code
            if expDt == Bool then do
                trueLabel  <- newLabel
                falseLabel <- newLabel
                jumpingCode expL trueLabel falseLabel

                generate $ PutLabel trueLabel "true label for assignment"
                generate $ Assign accAddr (Constant $ ValBool True)
                generate $ Goto nextLabel

                generate $ PutLabel falseLabel "false label for assignment"
                generate $ Assign accAddr (Constant $ ValBool False)

            else do
                expAddr <- linearizeExpression expL
                generate $ Assign accAddr expAddr

        StReturn expL -> linearizeExpression expL >>= generate . Return

        StFunctionDef idnL _ block -> do
            blockWdt <- liftM fromJust $ getsSymbol (lexInfo idnL) blockWidth

            generate $ BeginFunction blockWdt
            enterScope
            linearizeStatements block
            exitScope
            generate $ EndFunction

        StProcedureCall idnL prmLs -> do
            prmAddrs <- mapM linearizeExpression (reverse prmLs)
            mapM_ (generate . PushParameter) prmAddrs
            generate $ PCall (lexInfo idnL) (length prmAddrs)
            generate . PopParameters $ length prmAddrs

        StRead accL -> do
            tab <- gets table
            let accDt = processAccessChecker tab accL
            getAccessAddress accL >>= generate . case accDt of
                Int   -> ReadInt
                Float -> ReadFloat
                Bool  -> ReadBool
                Char  -> ReadChar

        StPrint expL -> do
            tab <- gets table
            let expDt          = processExpressionChecker tab expL
                LitString strL = lexInfo expL
                idn            = show $ lexInfo strL

            if expDt == String
                then getsSymbol idn (offset &&& width) >>=
                    generate . uncurry PrintString . fromJust
                else linearizeExpression expL >>= generate . case expDt of
                    Int   -> PrintInt
                    Float -> PrintFloat
                    Bool  -> PrintBool
                    Char  -> PrintChar

        StIf expL trueBlock falseBlock -> do
            trueLabel  <- newLabel
            falseLabel <- newLabel
            endLabel   <- newLabel

            jumpingCode expL trueLabel falseLabel

            generate $ PutLabel trueLabel "then label for `if`"
            enterScope
            linearizeStatements trueBlock
            exitScope
            generate $ Goto endLabel

            generate $ PutLabel falseLabel "else label for `if`"
            enterScope
            linearizeStatements falseBlock
            exitScope

            generate $ PutLabel endLabel "end of `if`"

        StCase expL whnLs othrBlock -> do
            testLabel <- newLabel
            othrLabel <- newLabel

            expAddr <- linearizeExpression expL
            generate $ Goto testLabel

            whenLabels <- forM whnLs $ \(Lex (When _ whnBlock) whnPosn) -> do
                whnLabel <- newLabel
                generate $ PutLabel whnLabel $ "when label for `case` in line " ++ show (row whnPosn)

                enterScope
                linearizeStatements whnBlock
                exitScope
                generate $ Goto nextLabel

                return whnLabel

            -- Only when there is an 'otherwise'
            unless (null othrBlock) $ do
                generate $ PutLabel othrLabel "otherwise label for `case`"
                enterScope
                linearizeStatements othrBlock
                exitScope
                generate $ Goto nextLabel

            generate $ PutLabel testLabel "test label for `case`"

            -- For every when, with the block's label
            forM_ (zip whenLabels whnLs) $ \(whnLabel, Lex (When whnExpLs _) _) ->
                forM_ whnExpLs $ \whnExpL -> do
                    whnAddrs <- linearizeExpression whnExpL
                    generate $ IfGoto EQ expAddr whnAddrs whnLabel

            -- Only jump when there is an 'otherwise'
            unless (null othrBlock) . generate $ Goto othrLabel

        StLoop befBlock expL aftBlock -> do
            befLabel <- newLabel
            aftLabel <- newLabel

            generate $ PutLabel befLabel "before block for `loop`"
            enterScope >> enterLoop befLabel nextLabel
            linearizeStatements befBlock
            exitScope

            jumpingCode expL aftLabel nextLabel

            generate $ PutLabel aftLabel "after block for `loop`"
            enterScope
            linearizeStatements aftBlock
            exitLoop >> exitScope

            generate $ Goto befLabel

        StFor idnL expL block -> do
            condLabel  <- newLabel
            blockLabel <- newLabel

            enterScope >> enterLoop condLabel nextLabel

            off <- liftM fromJust $ getsSymbol (lexInfo idnL) offset
            let idnAddr = Name (lexInfo idnL) off

            let ExpBinary _ fromExpL toExpL = lexInfo expL
            fromAddr <- linearizeExpression fromExpL
            toAddr   <- linearizeExpression toExpL

            -- Initialize the 'for' variable
            generate $ Assign idnAddr fromAddr

            generate $ PutLabel condLabel "condition label for `for`"
            generate $ IfGoto GT idnAddr toAddr nextLabel

            generate $ PutLabel blockLabel "block label for `for`"
            linearizeStatements block
            exitLoop >> exitScope

            generate $ AssignBin idnAddr ADD idnAddr (Constant $ ValInt 1)
            generate $ Goto condLabel

        StBreak    -> currentLoop >>= generate . Goto . snd
        StContinue -> currentLoop >>= generate . Goto . fst

        _ -> return ()
        -- StVariableDeclaration
        -- StStructDefinition

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
        _ -> if isComparable op then do
                lAddr <- linearizeExpression lExpL
                rAddr <- linearizeExpression rExpL
                generate $ IfGoto (binaryToRelation op) lAddr rAddr trueLabel
                generate $ Goto falseLabel
            else void $ linearizeExpression expL

    ExpUnary (Lex op _) unExpL -> case op of
        OpNot -> jumpingCode unExpL falseLabel trueLabel
        _     -> void $ linearizeExpression expL

    Variable accL -> do
        accAddr <- getAccessAddress accL
        generate $ IfTrueGoto accAddr trueLabel

    _ -> error "TACGenerator.jumpingCode: should not get jumping code for non-boolean expressions"

linearizeExpression :: Lexeme Expression -> TACGenerator (Address)
linearizeExpression (Lex exp _) = case exp of

    LitInt    v -> do
        resTemp <- newTemporary
        generate $ Assign resTemp (Constant . ValInt   $ lexInfo v)
        return resTemp

    LitFloat  v -> do
        resTemp <- newTemporary
        generate $ Assign resTemp (Constant . ValFloat $ lexInfo v)
        return resTemp

    LitBool   v -> do
        resTemp <- newTemporary
        generate $ Assign resTemp (Constant . ValBool  $ lexInfo v)
        return resTemp

    LitChar   v -> do
        resTemp <- newTemporary
        generate $ Assign resTemp (Constant . ValChar  $ lexInfo v)
        return resTemp

    LitString _ -> error "TACGenerator.linearizeExpression: should not get address for a String"

    Variable accL -> do
        accAddr <- getAccessAddress accL

        resTemp <- newTemporary
        generate $ Assign resTemp accAddr
        return resTemp

    FunctionCall idnL prmLs -> do
        resTemp  <- newTemporary
        prmAddrs <- mapM linearizeExpression (reverse prmLs)

        mapM_ (generate . PushParameter) prmAddrs
        generate $ FCall resTemp (lexInfo idnL) (length prmAddrs)
        return resTemp

    ExpBinary (Lex op _) lExpL rExpL -> do
        lAddr <- linearizeExpression lExpL
        rAddr <- linearizeExpression rExpL
        resTemp <- newTemporary
        generate $ AssignBin resTemp (binaryToBinOperator op) lAddr rAddr
        return resTemp

    ExpUnary (Lex op _) expL -> do
        expAddr <- linearizeExpression expL
        resTemp <- newTemporary
        generate $ AssignUn resTemp (unaryToUnOperator op) expAddr
        return resTemp

--------------------------------------------------------------------------------

getAccessAddress :: Lexeme Access -> TACGenerator Address
getAccessAddress accL = do
    -- It works only for Variable for now
    let deepZpp                 = deepAccess $ focusAccess accL
        VariableAccess deepIdnL = lexInfo $ defocusAccess deepZpp
        deepIdn                 = lexInfo deepIdnL
    (deepOff, deepDtL) <- liftM (fromJust) $ getsSymbol deepIdn (offset &&& dataType)

    off <- calculateRelativeOffset deepZpp deepOff (lexInfo deepDtL)
    return $ Name deepIdn off

----------------------------------------

calculateRelativeOffset :: AccessZipper -> Offset -> DataType -> TACGenerator Offset
calculateRelativeOffset accZ off dt = case defocusAccess `fmap` backAccess accZ of

    Just accL -> case lexInfo accL of

        ArrayAccess _ _ -> return off

        StructAccess _ fldIdnL -> do
            tab <- liftM (fromJust . fromJust) $ getsSymbol (toIdentifier dt) fields
            let (fldOff, fldDtL) = ((offset &&& dataType) . fromJust) $ lookup (lexInfo fldIdnL) tab

            calculateRelativeOffset (fromJust $ backAccess accZ) (off + fldOff) (lexInfo fldDtL)

    Nothing -> return off

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
