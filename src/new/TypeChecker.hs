module TypeChecker
    ( TypeState

    , TypeChecker
    --, buildTypeChecker
    --, runProgramTypeChecker
    , processTypeChecker
    ) where

import           Error
import           Program
import           SappMonad
import           SymbolTable

import           Control.Arrow             ((&&&))
import           Control.Monad             (guard, liftM, unless, void, when,
                                            (>=>))
import           Control.Monad.RWS         (RWS, lift, runRWS)
import           Control.Monad.State       (gets, modify)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Control.Monad.Writer      (tell)
import           Data.Foldable             as DF (all, and, forM_, mapM_)
import           Data.Functor              ((<$>))
import           Data.Maybe                (fromJust, fromMaybe, isJust)
import           Data.Sequence             as DS (Seq, empty, length, zipWith)
import           Data.Traversable          (mapM)
import           Prelude                   hiding (all, and, exp, length,
                                                 mapM, mapM_, zipWith)
import qualified Prelude                   as P (length)

--------------------------------------------------------------------------------

type TypeChecker = RWS SappReader SappWriter TypeState

--------------------------------------------------------------------------------
-- State

data TypeState = TypeState
    { table        :: SymbolTable
    , stack        :: Stack Scope
    , scopeId      :: ScopeNum
    , ast          :: Program
    , funcStack    :: Stack (Identifier, DataType, ScopeNum)
    , stringOffset :: Offset
    }

----------------------------------------
-- Instances

instance SappState TypeState where
    getTable   = table
    getStack   = stack
    getScopeId = scopeId
    getAst     = ast
    putTable   tab s = s { table   = tab }
    putStack   stk s = s { stack   = stk }
    putScopeId sc  s = s { scopeId = sc  }
    putAst     as  s = s { ast     = as  }

instance Show TypeState where
    show = showSappState

----------------------------------------
-- Initial

initialState :: TypeState
initialState = TypeState
    { table        = emptyTable
    , stack        = topStack
    , scopeId      = topScopeNum
    , ast          = Program empty
    , funcStack    = singletonStack ("sapphire", Void, topScopeNum)
    , stringOffset = 0
    }

--------------------------------------------------------------------------------
-- Building the Monad

buildTypeChecker :: SappWriter -> SymbolTable -> Program -> TypeChecker ()
buildTypeChecker w tab program@(Program block) = do
    modify $ \s -> s { table = tab, ast = program }
    tell w
    typeCheckStatements block

--------------------------------------------------------------------------------
-- Using the Monad

processTypeChecker :: SappWriter -> SymbolTable -> Program -> (TypeState, SappWriter)
processTypeChecker w tab = runProgramTypeChecker . buildTypeChecker w tab

runProgramTypeChecker :: TypeChecker a -> (TypeState, SappWriter)
runProgramTypeChecker = (\(_,s,w) -> (s,w)) . runTypeChecker

runTypeCheckerWithReader :: SappReader -> TypeChecker a -> (a, TypeState, SappWriter)
runTypeCheckerWithReader r = flip (flip runRWS r) initialState

runTypeChecker :: TypeChecker a -> (a, TypeState, SappWriter)
runTypeChecker = runTypeCheckerWithReader initialReader

getTypeChecker :: TypeChecker a -> a
getTypeChecker = (\(v,_,_) -> v) . runTypeChecker

getWriter :: TypeChecker a -> SappWriter
getWriter = (\(_,_,w) -> w) . runTypeChecker

getState :: TypeChecker a -> TypeState
getState = (\(_,s,_) -> s) . runTypeChecker

--------------------------------------------------------------------------------
-- Monad handling

enterFunction :: Identifier -> DataType -> TypeChecker ()
enterFunction idnL dt = do
    currentId <- currentScope
    modify $ \s -> s { funcStack = push (idnL, dt, currentId) (funcStack s) }

exitFunction :: TypeChecker ()
exitFunction = modify $ \s -> s { funcStack = pop $ funcStack s }

currentFunction :: TypeChecker (Identifier, DataType, ScopeNum)
currentFunction = gets (top . funcStack)

--------------------------------------------------------------------------------
-- Statements

typeCheckStatements :: StBlock -> TypeChecker ()
typeCheckStatements = mapM_ typeCheckStatement

typeCheckStatement :: Lexeme Statement -> TypeChecker ()
typeCheckStatement (Lex st posn) = case st of

    StAssign accL expL -> void $ runMaybeT $ do
        expDt           <- lift $ typeCheckExpression expL
        (accIdn, accDt) <- accessDataType accL

        -- Checking for TypeErrors
        guard (isValid accDt)
        guard (isValid expDt)
        unless (accDt == expDt) $ tellSError posn (InvalidAssignType accIdn accDt expDt)

    StStructDefinition _ ->  enterScope >> exitScope        -- For scopeStack maintenance

    StReturn expL -> void $ runMaybeT $ do
        expDt <- lift $ typeCheckExpression expL
        (idn, retDt, funcScopeId) <- lift currentFunction

        unlessGuard (retDt /= Void) $ if funcScopeId /= topScopeNum
            then tellSError posn (ReturnInProcedure expDt idn)
            else tellSError posn (StaticError "can't return in top level")

        -- Checking for TypeError
        guard (isValid expDt)
        unlessGuard (retDt == expDt) $ tellSError posn (ReturnType retDt expDt idn)

        modifySymbol idn (\s -> s { returned = True })

    StFunctionDef (Lex idn _) _ block -> do
        dtL <- liftM fromJust $ getsSymbol idn returnType
        enterScope
        enterFunction idn (lexInfo dtL)
        typeCheckStatements block
        exitFunction
        exitScope

    StProcedureCall idnL expLs -> void $ runMaybeT $ checkArguments idnL expLs False

    StRead accL -> void $ runMaybeT $ do
        (accIdn, accDt) <- accessDataType accL
        guard (isValid accDt)
        unless (isScalar accDt) $ tellSError posn (ReadNonReadable accDt accIdn)

    StPrint exprL -> void $ runMaybeT $ do
        dt <- lift $ typeCheckExpression exprL
        guard (isValid dt)
        unless (dt == String || isScalar dt) $ tellSError posn (PrintNonPrintable dt)

    StIf expL trueBlock falseBlock -> do
        expDt <- typeCheckExpression expL
        runMaybeT $ do
            guard (isValid expDt)
            when (expDt /= Bool) $ tellSError posn (ConditionDataType expDt)

        enterScope
        typeCheckStatements trueBlock
        exitScope

        enterScope
        typeCheckStatements falseBlock
        exitScope

    StCase expL whnLs othrBlock -> do
        expDt <- typeCheckExpression expL
        unless (isScalar expDt) $ tellSError posn (CaseNonCaseable expDt)

        -- For every when
        forM_ whnLs $ \(Lex (When whnExpLs whnBlock) whnP) -> do
            -- For every expression in said when
            forM_ whnExpLs $ typeCheckExpression >=> \whnExpDt -> runMaybeT $ do
                guard (isValid whnExpDt)
                guard (isValid expDt)
                unless (whnExpDt == expDt) $ tellSError whnP (CaseWhenDataType expDt whnExpDt)

            -- Check statements
            enterScope
            typeCheckStatements whnBlock
            exitScope

        enterScope
        typeCheckStatements othrBlock
        exitScope

    StLoop befBlock expL aftBlock -> do
        expDt <- typeCheckExpression expL
        runMaybeT $ do
            guard (isValid expDt)
            when (expDt /= Bool) $ tellSError posn (ConditionDataType expDt)

        enterScope
        typeCheckStatements befBlock
        exitScope

        enterScope
        typeCheckStatements aftBlock
        exitScope

    StFor _ expL block -> do
        expDt <- typeCheckExpression expL
        runMaybeT $ do
            guard (isValid expDt)
            when (expDt /= Range) $ tellSError posn (ForInDataType expDt)

        enterScope
        typeCheckStatements block
        exitScope

    _ -> return ()
    --StVariableDeclaration
    --StBreak
    --StContinue

typeCheckExpression :: Lexeme Expression -> TypeChecker DataType
typeCheckExpression (Lex exp posn) = case exp of

    LitInt    _ -> return Int
    LitFloat  _ -> return Float
    LitBool   _ -> return Bool
    LitChar   _ -> return Char

    LitString strL -> do
        strOff <- gets stringOffset
        let strWdt = P.length $ lexInfo strL
            info = emptySymInfo
                { dataType   = fillLex String
                , category   = CatConstant
                , offset     = strOff
                , width      = strWdt
                , used       = True
                , scopeStack = topStack
                , defPosn    = lexPosn strL
                }
        -- Set the new global offset
        modify $ \s -> s { stringOffset = strOff + strWdt}
        -- '$' means it's a constant
        addSymbol "$String" info
        return String

    Variable accL -> liftM (maybe TypeError snd) $ runMaybeT $ accessDataType accL

    FunctionCall idnL expLs -> liftM (fromMaybe TypeError) $ runMaybeT $ checkArguments idnL expLs True

    ExpBinary (Lex op _) lExpL rExpL -> liftM (fromMaybe TypeError) $ runMaybeT $ do
        lDt <- lift $ typeCheckExpression lExpL
        rDt <- lift $ typeCheckExpression rExpL
        let expDt = binaryOperation op (lDt, rDt)

        -- Checking for TypeErrors
        guard (isValid lDt)
        guard (isValid rDt)

        unlessGuard (isJust expDt) $ tellSError posn (BinaryTypes op (lDt, rDt))

        return (fromJust expDt)

    ExpUnary (Lex op _) expL -> liftM (fromMaybe TypeError) $ runMaybeT $ do
        dt <- lift $ typeCheckExpression expL
        let expDt = unaryOperation op dt

        -- Checking for TypeError
        guard (isValid dt)

        unlessGuard (isJust expDt) $ tellSError posn (UnaryTypes op dt)

        return (fromJust expDt)

--------------------------------------------------------------------------------

checkArguments :: Lexeme Identifier -> Seq (Lexeme Expression) -> Bool -> MaybeT TypeChecker DataType
checkArguments (Lex idn posn) args func = do
    maySymI <- getsSymbol idn (\sym -> (symbolCategory sym, returnType sym, paramTypes sym))

    unlessGuard (isJust maySymI) $ tellSError posn (NotDefined idn)

    let (cat, Lex dt _, prms) = fromJust maySymI

    -- When is not a function
    unlessGuard (cat == CatFunction) $ tellSError posn (WrongCategory idn CatFunction cat)

    markUsed idn

    if func
        -- When is a procedure, and should be a function
        then when (dt == Void) $ tellSError posn (ProcedureInExpression idn)
        -- When is a function, and should be a procedure
        else when (dt /= Void) $ tellSError posn (FunctionAsStatement idn)

    -- Typecheck arguments
    aDts <- lift $ mapM typeCheckExpression args
    let pDts = fmap lexInfo prms

    -- Must have same length
    unlessGuard (length args == length pDts) $ tellSError posn (FunctionArguments idn pDts aDts)

    -- Checking for TypeErrors
    guard (all isValid aDts)
    -- Must have same DataTypes
    unlessGuard (and $ zipWith (==) pDts aDts) $ tellSError posn (FunctionArguments idn pDts aDts)

    return dt

--------------------------------------------------------------------------------

accessDataType :: Lexeme Access -> MaybeT TypeChecker (Identifier, DataType)
accessDataType accL = do
    let deepZpp                 = deepAccess $ focusAccess accL
        VariableAccess deepIdnL = lexInfo $ defocusAccess deepZpp
        deepIdn                 = lexInfo deepIdnL
    maySymI <- getsSymbol deepIdn ((lexInfo . dataType) &&& symbolCategory)

    unlessGuard (isJust maySymI) $ tellSError (lexPosn deepIdnL) (NotDefined deepIdn)

    let (dt, cat) = fromJust maySymI

    -- Checking that it is a variable
    unlessGuard (cat == CatInfo) $ tellSError (lexPosn accL) (WrongCategory deepIdn CatInfo cat)

    markUsed deepIdn

    dt' <- constructDataType deepIdnL deepZpp dt
    return (deepIdn, dt')

----------------------------------------

constructDataType :: Lexeme Identifier -> AccessZipper -> DataType -> MaybeT TypeChecker DataType
constructDataType idnL accZ dt = case defocusAccess <$> backAccess accZ of

    Just accL -> case lexInfo accL of

        ArrayAccess  _ expL -> do
            unlessGuard (isArray dt) $ tellSError (lexPosn idnL) (AccessNonArray (lexInfo idnL) dt)

            expDt <- lift $ typeCheckExpression expL

            unlessGuard (expDt == Int) $ tellSError (lexPosn expL) (IndexDataType (lexInfo expL) expDt)

            constructDataType idnL (fromJust $ backAccess accZ) (arrayInnerDataType dt)

        StructAccess _ fldIdnL -> do
            unlessGuard (isStruct dt) $ tellSError (lexPosn idnL) (AccessNonStruct (lexInfo idnL) dt)

            let mayFldDt = lexInfo <$> fieldInStruct dt (lexInfo fldIdnL)
            unlessGuard (isJust mayFldDt) $ tellSError (lexPosn fldIdnL) (StructNoField dt (lexInfo fldIdnL))

            constructDataType idnL (fromJust $ backAccess accZ) (fromJust mayFldDt)

    Nothing -> return dt
