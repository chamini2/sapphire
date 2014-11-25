{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-|
 - TAC Generator Monad
 -
 - This monad needs to generate the necessary intermediate representation (IR)
 - in this case the three-reference code; store it temporarily as a structure for
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
import           Language.Sapphire.TypeChecker (processAccessChecker,
                                                processExpressionChecker)

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (guard, liftM, unless, void)
import           Control.Monad.State           (State (), execState, get, gets,
                                                modify)
import           Control.Monad.Trans           (lift)
import           Control.Monad.Trans.Maybe     (runMaybeT)
import           Data.Foldable                 (foldl', forM_, mapM_, toList, sum)
import           Data.Maybe                    (fromJust, isJust)
import           Data.Sequence                 (Seq, empty, fromList,
                                                null, reverse, zip,
                                                (><), (|>))
import           Data.Traversable              (forM, mapM)
import           Prelude                       hiding (Ordering (..), exp,
                                                length, lookup, mapM, sum, mapM_,
                                                null, reverse, zip)

--------------------------------------------------------------------------------

type TACGenerator = State TACState

--------------------------------------------------------------------------------
-- State

data TACState = TACState
    { table       :: SymbolTable
    , stack       :: Stack Scope
    , scopeId     :: Scope
    , ast         :: Program
    , code        :: TAC
    , tempWidth   :: Offset
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
    , stack       = globalStack
    , scopeId     = globalScope
    , ast         = Program empty
    , code        = empty
    , tempWidth  = 0
    , tempSerial  = 0
    , labelSerial = 0
    , loopStack   = emptyStack
    }

--------------------------------------------------------------------------------
-- Building the Monad

buildTACGenerator :: SymbolTable -> Program -> TACGenerator ()
buildTACGenerator tab program@(Program block) = do
    modify $ \s -> s { table = tab, ast = program }
    void $ linearizeStatements block

--------------------------------------------------------------------------------
-- Using the Monad

processTACGenerator :: SymbolTable -> Program -> (TACState, Seq TAC)
processTACGenerator state = (\s -> (s, makeBasicBlocks $ code s)) . generateTAC . buildTACGenerator state

generateTAC :: TACGenerator a -> TACState
generateTAC = flip execState initialState

--------------------------------------------------------------------------------
-- Monad handling

generate :: Instruction -> TACGenerator ()
generate ins = modify $ \s -> s { code = code s |> ins }

enterLoop :: Label -> Label -> TACGenerator ()
enterLoop startLoop endLoop = modify $ \s -> s { loopStack = push (startLoop, endLoop) (loopStack s) }

exitLoop :: TACGenerator ()
exitLoop = modify $ \s -> s { loopStack = pop (loopStack s) }

currentLoop :: TACGenerator (Label, Label)
currentLoop = gets (top . loopStack)

----------------------------------------

newTemporary :: TACGenerator Location
newTemporary = do
    (ser, off) <- gets ((succ . tempSerial) &&& tempWidth)
    modify $ \s -> s { tempSerial = ser, tempWidth = off - 4 } -- word size = 4
    let idn = "_tmp" ++ show ser
    return $ Address idn off False

newLabel :: TACGenerator Label
newLabel = do
    ser <- gets (succ . labelSerial)
    modify $ \s -> s { labelSerial = ser }
    return $ "L" ++ show ser

--------------------------------------------------------------------------------
-- Statements

linearizeStatements :: StBlock -> TACGenerator ()
linearizeStatements = mapM_ $ \stL -> do
    nextLabel <- newLabel
    linearizeStatement nextLabel stL
    generate $ PutLabel nextLabel
    generate $ Comment $ "next statement of line " ++ show (row $ lexPosn stL)  ++ ", " ++ show (lexInfo stL)

linearizeStatement :: Label -> Lexeme Statement -> TACGenerator ()
linearizeStatement nextLabel (Lex st posn) = do
    generate $ Comment showStatement
    case st of

        StAssign accL expL ->  do
            state <- get
            let expDt = processExpressionChecker state expL
            accAddr <- accessLocation accL

            -- When assigning a Boolean expression, use jumping code
            if expDt == Bool then do
                trueLabel  <- newLabel
                falseLabel <- newLabel
                jumpingCode expL trueLabel falseLabel

                trueTemp <- newTemporary
                falseTemp <- newTemporary

                generate $ PutLabel trueLabel
                generate $ Comment $ "true label for " ++ showStatement

                generate $ LoadConstant trueTemp $ ValBool True
                generate $ Store trueTemp 0 accAddr

                generate $ Goto nextLabel

                generate $ PutLabel falseLabel
                generate $ Comment $ "false label for " ++ showStatement

                generate $ LoadConstant falseTemp $ ValBool False
                generate $ Store falseTemp 0 accAddr

            else do
                expAddr <- linearizeExpression expL
                generate $ Store expAddr 0 accAddr

        StReturn mayExpL ->  (=<<) (generate . Return) . runMaybeT $ do
            guard (isJust mayExpL)
            lift . linearizeExpression $ fromJust mayExpL

        StFunctionDef idnL _ block -> do
            blockOff <- liftM (negate . fromJust) $ getsSymbol (lexInfo idnL) blockWidth

            generate $ PutLabel (lexInfo idnL)

            -- Calculate the width with temporaries included
            befCode <- gets code
            modify $ \s -> s { code = empty, tempWidth = blockOff }

            enterScope
            linearizeStatements block
            exitScope

            (aftCode, blockOff') <- gets (code &&& (negate . tempWidth))

            -- Build the code again
            let code' = ((befCode |> BeginFunction (blockOff' - 8)) >< aftCode) |> EndFunction
            modify $ \s -> s { code = code' }

        StProcedureCall idnL prmLs -> do
            state <- get
            prmWdt <- liftM sum $ mapM (dataTypeWidth . processExpressionChecker state) prmLs

            mapM linearizeExpression (reverse prmLs) >>=
                mapM_ (generate . PushParam)

            generate $ PCall (lexInfo idnL)
            generate $ PopParams prmWdt

        StRead accL -> do
            state <- get
            let accDt = processAccessChecker state accL
            accessLocation accL >>= generate . case accDt of
                Int   -> ReadInt
                Float -> ReadFloat
                Bool  -> ReadBool
                Char  -> ReadChar
                _     -> error "TACGenerator.linearizeStatement.StRead: unreadble DataType"

        StPrint expL -> do
            state <- get
            let expDt          = processExpressionChecker state expL
                LitString strL = lexInfo expL
                idn            = show $ lexInfo strL

            if expDt == String
                then getsSymbol idn offset >>=
                    generate . PrintString . ("_str"++) . show . fromJust
                else linearizeExpression expL >>= generate . case expDt of
                    Int   -> PrintInt
                    Float -> PrintFloat
                    Bool  -> PrintBool
                    Char  -> PrintChar
                    _     -> error "TACGenerator.linearizeStatement.StPrint: unprintable DataType"

        StIf expL trueBlock falseBlock -> do
            trueLabel  <- newLabel
            falseLabel <- newLabel

            jumpingCode expL trueLabel falseLabel

            generate $ PutLabel trueLabel
            generate $ Comment $ "then label for " ++ showStatement
            enterScope
            linearizeStatements trueBlock
            exitScope

            generate $ Goto nextLabel

            generate $ PutLabel falseLabel
            generate $ Comment $ "else label for " ++ showStatement
            enterScope
            linearizeStatements falseBlock
            exitScope

        StCase expL whnLs othrBlock -> do
            testLabel <- newLabel
            othrLabel <- newLabel

            expAddr <- linearizeExpression expL
            generate $ Goto testLabel

            whenLabels <- forM whnLs $ \(Lex (When _ whnBlock) whnP) -> do
                whnLabel <- newLabel
                generate $ PutLabel whnLabel
                generate $ Comment $ "when " ++ show whnP ++ " label for " ++ showStatement

                enterScope
                linearizeStatements whnBlock
                exitScope
                generate $ Goto nextLabel

                return whnLabel

            -- Only when there is an 'otherwise'
            unless (null othrBlock) $ do
                generate $ PutLabel othrLabel
                generate $ Comment $ "otherwise label for " ++ showStatement
                enterScope
                linearizeStatements othrBlock
                exitScope
                generate $ Goto nextLabel

            generate $ PutLabel testLabel
            generate $ Comment $ "test label for " ++ showStatement

            -- For every when, with the block's label
            forM_ (zip whenLabels whnLs) $ \(whnLabel, Lex (When whnExpLs _) _) ->
                forM_ whnExpLs $ \whnExpL -> do
                    whnAddr <- linearizeExpression whnExpL

                    testTemp <- newTemporary
                    generate $ BinaryOp testTemp (Rel EQ) expAddr whnAddr

                    generate $ IfTrueGoto testTemp whnLabel

            -- Only jump when there is an 'otherwise'
            unless (null othrBlock) . generate $ Goto othrLabel

        StLoop befBlock expL aftBlock -> do
            befLabel <- newLabel
            aftLabel <- newLabel

            generate $ PutLabel befLabel
            generate $ Comment $ "before block for " ++ showStatement
            enterScope >> enterLoop befLabel nextLabel
            linearizeStatements befBlock
            exitScope

            jumpingCode expL aftLabel nextLabel

            generate $ PutLabel aftLabel
            generate $ Comment $ "after block for " ++ showStatement
            enterScope
            linearizeStatements aftBlock
            exitLoop >> exitScope

            generate $ Goto befLabel

        StFor idnL expL block -> do
            condLabel  <- newLabel
            blockLabel <- newLabel

            enterScope >> enterLoop condLabel nextLabel

            off <- liftM fromJust $ getsSymbol (lexInfo idnL) offset

            let idnAddr = Address (lexInfo idnL) off False

            let ExpBinary _ fromExpL toExpL = lexInfo expL
            fromAddr <- linearizeExpression fromExpL
            toAddr   <- linearizeExpression toExpL

            -- Initialize the 'for' variable
            generate $ Assign idnAddr fromAddr

            generate $ PutLabel condLabel
            generate $ Comment $ "condition label for " ++ showStatement

            testTemp <- binaryToBinOperator OpGreat idnAddr toAddr

            generate $ IfTrueGoto testTemp nextLabel

            generate $ PutLabel blockLabel
            generate $ Comment $ "block label for " ++ showStatement
            linearizeStatements block
            exitLoop >> exitScope

            stepTemp <- newTemporary
            generate $ LoadConstant stepTemp $ ValInt 1

            generate $ BinaryOp idnAddr ADD idnAddr stepTemp
            generate $ Goto condLabel

        StBreak    -> currentLoop >>= generate . Goto . snd
        StContinue -> currentLoop >>= generate . Goto . fst

        _ -> return ()
        -- StVariableDeclaration
        -- StStructDefinition
    where
        showStatement = "line " ++ show (row posn) ++ ", " ++ show st

--------------------------------------------------------------------------------
-- Expressions

jumpingCode :: Lexeme Expression -> Label -> Label -> TACGenerator ()
jumpingCode expL@(Lex exp _) trueLabel falseLabel = case exp of

    LitBool v -> generate $ Goto (if lexInfo v then trueLabel else falseLabel)

    ExpBinary (Lex op _) lExpL rExpL -> case op of
        OpOr  -> do
            rightLabel <- newLabel
            jumpingCode lExpL trueLabel rightLabel
            generate $ PutLabel rightLabel
            generate $ Comment "right operand of `or`"
            jumpingCode rExpL trueLabel falseLabel
        OpAnd -> do
            rightLabel <- newLabel
            jumpingCode lExpL rightLabel falseLabel
            generate $ PutLabel rightLabel
            generate $ Comment "right operand of `and`"
            jumpingCode rExpL trueLabel falseLabel
        _ -> if isComparable op then do
                testAddr <- linearizeExpression expL
                generate $ IfTrueGoto testAddr trueLabel
                generate $ Goto falseLabel
            else void $ linearizeExpression expL

    ExpUnary (Lex op _) unExpL -> case op of
        OpNot -> jumpingCode unExpL falseLabel trueLabel
        _     -> void $ linearizeExpression expL

    Variable accL -> do
        accAddr <- accessLocation accL
        generate $ IfTrueGoto accAddr trueLabel
        generate $ Goto falseLabel

    _ -> error "TACGenerator.jumpingCode: should not get jumping code for non-boolean expressions"

linearizeExpression :: Lexeme Expression -> TACGenerator Location
linearizeExpression (Lex exp _) = case exp of

    LitInt    v -> linearizeLiteral . ValInt   $ lexInfo v
    LitFloat  v -> linearizeLiteral . ValFloat $ lexInfo v
    LitBool   v -> linearizeLiteral . ValBool  $ lexInfo v
    LitChar   v -> linearizeLiteral . ValChar  $ lexInfo v
    LitString _ -> error "TACGenerator.linearizeExpression: should not get address for a String"

    Variable accL -> do
        accAddr <- accessLocation accL

        resTemp <- newTemporary
        generate $ Load resTemp 0 accAddr
        return resTemp

    FunctionCall idnL prmLs -> do
        state <- get
        prmWdt <- liftM sum $ mapM (dataTypeWidth . processExpressionChecker state) prmLs

        mapM linearizeExpression (reverse prmLs) >>=
            mapM_ (generate . PushParam)

        resTemp <- newTemporary
        generate $ FCall (lexInfo idnL) resTemp
        generate $ PopParams prmWdt

        return resTemp

    ExpBinary (Lex op _) lExpL rExpL -> do
        lAddr <- linearizeExpression lExpL
        rAddr <- linearizeExpression rExpL

        binaryToBinOperator op lAddr rAddr

    ExpUnary (Lex op _) expL -> do
        expAddr <- linearizeExpression expL
        resTemp <- newTemporary

        case op of
            OpNegate -> do
                leftTemp <- newTemporary
                generate $ LoadConstant leftTemp $ ValInt 0
                generate $ BinaryOp resTemp SUB leftTemp expAddr
            OpNot -> generate $ UnaryOp resTemp NOT expAddr

        return resTemp

    where
        linearizeLiteral :: Value -> TACGenerator Location
        linearizeLiteral v = do
            temp <- newTemporary
            generate $ LoadConstant temp v
            return temp


binaryToBinOperator :: Binary -> Location -> Location -> TACGenerator Location
binaryToBinOperator op lAddr rAddr = do
    resTemp <- newTemporary

    case op of
        OpPlus    -> generate $ BinaryOp resTemp ADD      lAddr rAddr
        OpMinus   -> generate $ BinaryOp resTemp SUB      lAddr rAddr
        OpTimes   -> generate $ BinaryOp resTemp MUL      lAddr rAddr
        OpDivide  -> generate $ BinaryOp resTemp DIV      lAddr rAddr
        OpModulo  -> generate $ BinaryOp resTemp MOD      lAddr rAddr
        OpPower   -> generate $ BinaryOp resTemp POW      lAddr rAddr
        OpOr      -> generate $ BinaryOp resTemp OR       lAddr rAddr
        OpAnd     -> generate $ BinaryOp resTemp AND      lAddr rAddr
        OpEqual   -> generate $ BinaryOp resTemp (Rel EQ) lAddr rAddr
        OpUnequal -> generate $ BinaryOp resTemp (Rel NE) lAddr rAddr
        OpLess    -> generate $ BinaryOp resTemp (Rel LT) lAddr rAddr
        OpLessEq  -> do
            ltTemp <- newTemporary
            generate $ BinaryOp ltTemp (Rel LT) lAddr  rAddr

            eqTemp <- newTemporary
            generate $ BinaryOp eqTemp (Rel EQ) lAddr  rAddr

            generate $ BinaryOp resTemp  OR     ltTemp eqTemp
        OpGreat   -> do
            ltTemp <- newTemporary
            generate $ BinaryOp ltTemp (Rel LT) lAddr rAddr

            nlTemp <- newTemporary
            generate $ UnaryOp nlTemp NOT ltTemp

            eqTemp <- newTemporary
            generate $ BinaryOp eqTemp (Rel EQ) lAddr rAddr

            neTemp <- newTemporary
            generate $ UnaryOp nlTemp NOT eqTemp

            generate $ BinaryOp resTemp  AND nlTemp neTemp
        OpGreatEq -> do
            ltTemp <- newTemporary
            generate $ BinaryOp ltTemp (Rel LT) lAddr rAddr

            generate $ UnaryOp resTemp NOT ltTemp
        _         -> error "TAC.binaryToBinOperator: trying to convert '@' or '..' to a intermediate code binary operator"
        -- OpFromTo
        -- OpBelongs

    return resTemp


--------------------------------------------------------------------------------

accessLocation :: Lexeme Access -> TACGenerator Location
accessLocation accL = do
    let deepZpp                 = deepAccess $ focusAccess accL
        VariableAccess deepIdnL = lexInfo $ defocusAccess deepZpp
        deepIdn                 = lexInfo deepIdnL
    (deepScp, deepOff, deepDtL) <- liftM fromJust $ getsSymbol deepIdn (\sym -> (top $ scopeStack sym, offset sym, dataType sym))

    -- Checking if it is a global variable
    let baseAddr = Address deepIdn deepOff (deepScp == globalScope)
    calculateAccessAddress baseAddr deepZpp (lexInfo deepDtL)
    where
        calculateAccessAddress :: Location -> AccessZipper -> DataType -> TACGenerator Location
        calculateAccessAddress baseAddr accZ dt = case fmap defocusAccess $ backAccess accZ of
            Just (Lex acc _) -> case acc of

                ArrayAccess _ expL -> do
                    let inDt = arrayInnerDataType dt

                    dtWdt   <- dataTypeWidth inDt
                    expAddr <- linearizeExpression expL

                    wdtTemp <- newTemporary
                    generate $ LoadConstant wdtTemp $ ValInt dtWdt

                    indTemp <- newTemporary
                    generate $ BinaryOp indTemp MUL expAddr wdtTemp

                    resTemp <- newTemporary
                    generate $ BinaryOp resTemp ADD indTemp baseAddr

                    calculateAccessAddress resTemp (fromJust $ backAccess accZ) (arrayInnerDataType dt)

                StructAccess _ fldIdnL -> do
                    tab <- liftM (fromJust . fromJust) $ getsSymbol (toIdentifier dt) fields
                    let (fldOff, fldDtL) = ((offset &&& dataType) . fromJust) $ lookup (lexInfo fldIdnL) tab

                    fldTemp <- newTemporary
                    generate $ LoadConstant fldTemp $ ValInt fldOff

                    resTemp <- newTemporary
                    generate $ BinaryOp resTemp ADD fldTemp baseAddr

                    calculateAccessAddress resTemp (fromJust $ backAccess accZ) (lexInfo fldDtL)

                _ -> error "TACGenerator.calculateAccessAddress: unrecognized Access"

            Nothing -> return baseAddr

-- calculateAccessOffset :: AccessZipper -> Reference -> DataType -> TACGenerator Reference
-- calculateAccessOffset accZ offAddr dt = case defocusAccess <$> backAccess accZ of

--     Just accL -> case lexInfo accL of

--         ArrayAccess _ expL -> do
--             let inDt = arrayInnerDataType dt

--             dtWdt   <- dataTypeWidth inDt
--             expAddr <- linearizeExpression expL

--             -- wdtTemp <- newTemporary
--             indTemp <- newTemporary
--             resTemp <- newTemporary

--             -- generate $ Assign wdtTemp (Constant $ ValInt dtWdt)
--             generate $ AssignBin indTemp MUL expAddr (Constant $ ValInt dtWdt)
--             generate $ AssignBin resTemp ADD indTemp offAddr

--             calculateAccessOffset (fromJust $ backAccess accZ) resTemp (arrayInnerDataType dt)

--         StructAccess _ fldIdnL -> do
--             tab <- liftM (fromJust . fromJust) $ getsSymbol (toIdentifier dt) fields
--             let (fldOff, fldDtL) = ((offset &&& dataType) . fromJust) $ lookup (lexInfo fldIdnL) tab

--             -- fldTemp <- newTemporary
--             resTemp <- newTemporary

--             -- generate $ Assign fldTemp (Constant $ ValInt fldOff)
--             generate $ AssignBin resTemp ADD offAddr (Constant $ ValInt fldOff)

--             calculateAccessOffset (fromJust $ backAccess accZ) resTemp (lexInfo fldDtL)

--         _ -> error "TACGenerator.calculateAccessOffset: unrecognized Access"

--     Nothing -> return offAddr

----------------------------------------

dataTypeWidth :: DataType -> TACGenerator Width
dataTypeWidth dt = if isArray dt
    then do
        let Array inDtL sizL = dt
        inDtWdt <- dataTypeWidth $ lexInfo inDtL
        return $ inDtWdt * lexInfo sizL
    else liftM fromJust $ getsSymbol (toIdentifier dt) width

----------------------------------------

type Leader = Bool

makeBasicBlocks :: TAC -> Seq TAC
makeBasicBlocks = separateBlocks . markLeaders
    where
        separateBlocks :: Seq (Leader, Instruction) -> Seq TAC
        separateBlocks = fmap fromList . separate empty . toList
            where
                separate blcs = \case
                    []                 -> blcs
                    ((True, ins) : inss) -> separate (blcs |> blc) rest
                        where
                            blc  = ins : (map snd $ takeWhile (not . fst) inss)
                            rest = dropWhile (not . fst) inss
                    _ -> error "TACGenerator.makeBasicBlocks: non-leader instruction pattern-matched"

        markLeaders :: Seq Instruction -> Seq (Leader, Instruction)
        markLeaders = markLabels . markGotos

        markGotos :: Seq Instruction -> (Seq (Leader, Instruction), [Label])
        markGotos = (\(inss, lbs, _) -> (inss, lbs)) . foldl' func (empty, [], True)
            where
                func (inss, lbs, mrk) ins = if hasGoto ins
                    then (inss |> (mrk, ins), label ins : lbs, True)
                    else (inss |> (mrk, ins), lbs            , False)

        markLabels :: (Seq (Leader, Instruction), [Label]) -> Seq (Leader, Instruction)
        markLabels = uncurry (foldl' (flip func))
            where
                func lb = fmap $ \(mrk, ins) -> (mrk || isPutLabel ins && (lb == label ins), ins)
