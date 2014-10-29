module Language.Sapphire.TypeChecker
    ( TypeState

    , TypeChecker
    , processTypeChecker

    -- For usage of other modules
    , processExpressionChecker
    , processAccessChecker
    ) where

import           Language.Sapphire.Error
import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad
import           Language.Sapphire.SymbolTable

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (guard, liftM, unless, void,
                                                when, (>=>))
import           Control.Monad.Reader          (asks)
import           Control.Monad.RWS             (RWS, evalRWS, execRWS, lift)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Trans.Maybe     (MaybeT, runMaybeT)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (all, and, forM_, or)
import           Data.Functor                  ((<$>))
import           Data.Maybe                    (fromJust, fromMaybe, isJust)
import           Data.Sequence                 (Seq, empty, length, zipWith)
import           Data.Traversable              (forM, mapM)
import           Prelude                       hiding (all, and, exp, length,
                                                lookup, mapM, null, or, zipWith)

--------------------------------------------------------------------------------

type TypeChecker = RWS SappReader SappWriter TypeState

--------------------------------------------------------------------------------
-- State

data TypeState = TypeState
    { table     :: SymbolTable
    , stack     :: Stack Scope
    , scopeId   :: Scope
    , ast       :: Program
    , funcStack :: Stack (Identifier, DataType, Scope)
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
    , stack        = globalStack
    , scopeId      = globalScope
    , ast          = Program empty
    , funcStack    = emptyStack
    }

--------------------------------------------------------------------------------
-- Building the Monad

buildTypeChecker :: SappWriter -> SymbolTable -> Program -> TypeChecker ()
buildTypeChecker w tab program@(Program block) = do
    modify $ \s -> s { table = tab, ast = program }
    tell w
    void $ typeCheckStatements block

    syms <- liftM toSeq $ gets table

    -- Check warnings only when not suppressed
    flgs <- asks flags
    unless (SuppressWarnings `elem` flgs) $ checkTable syms

----------------------------------------

checkTable :: Seq (Identifier, Symbol) -> TypeChecker ()
checkTable syms = forM_ syms $ \(idn, sym) -> case symbolCategory sym of
        CatInfo     -> unless (used sym) $ tellWarn (defPosn sym) (DefinedNotUsed idn)
        CatType     -> unless (used sym) $ tellWarn (defPosn sym) (TypeDefinedNotUsed idn)
        CatFunction -> unless (used sym) $ tellWarn (defPosn sym) (FunctionDefinedNotUsed idn)

--------------------------------------------------------------------------------
-- Using the Monad

processTypeChecker :: SappReader -> SappWriter -> SymbolTable -> Program -> (TypeState, SappWriter)
processTypeChecker r w tab = runTypeChecker r . buildTypeChecker w tab

runTypeChecker :: SappReader -> TypeChecker a -> (TypeState, SappWriter)
runTypeChecker r = flip (flip execRWS r) initialState

----------------------------------------
-- Expression

processExpressionChecker :: SappState s => s -> Lexeme Expression -> DataType
processExpressionChecker st = evalExpressionChecker . buildExpressionChecker st

evalExpressionChecker :: TypeChecker DataType -> DataType
evalExpressionChecker = fst . flip (flip evalRWS initialReader) initialState

buildExpressionChecker :: SappState s => s -> Lexeme Expression -> TypeChecker DataType
buildExpressionChecker st expL = do
    modify $ putTable   (getTable   st)
    modify $ putStack   (getStack   st)
    modify $ putScopeId (getScopeId st)
    modify $ putAst     (getAst     st)
    typeCheckExpression expL

----------------------------------------
-- Access

processAccessChecker :: SappState s => s -> Lexeme Access -> DataType
processAccessChecker st = evalAccessChecker . buildAccessChecker st

evalAccessChecker :: TypeChecker DataType -> DataType
evalAccessChecker = fst . flip (flip evalRWS initialReader) initialState

buildAccessChecker :: SappState s => s -> Lexeme Access -> TypeChecker DataType
buildAccessChecker st accL = do
    modify $ putTable   (getTable   st)
    modify $ putStack   (getStack   st)
    modify $ putScopeId (getScopeId st)
    modify $ putAst     (getAst     st)
    liftM (snd . fromJust) . runMaybeT $ accessDataType accL

--------------------------------------------------------------------------------
-- Monad handling

enterFunction :: Identifier -> DataType -> TypeChecker ()
enterFunction idnL dt = do
    currentId <- currentScope
    modify $ \s -> s { funcStack = push (idnL, dt, currentId) (funcStack s) }

exitFunction :: TypeChecker ()
exitFunction = modify $ \s -> s { funcStack = pop $ funcStack s }

currentFunction :: TypeChecker (Identifier, DataType, Scope)
currentFunction = gets (top . funcStack)

--------------------------------------------------------------------------------
-- Statements

typeCheckStatements :: StBlock -> TypeChecker Returned
typeCheckStatements = liftM or . mapM typeCheckStatement

typeCheckStatement :: Lexeme Statement -> TypeChecker Returned
typeCheckStatement (Lex st posn) = case st of

    StAssign accL expL -> flip (>>) (return False) . runMaybeT $ do
        expDt           <- lift $ typeCheckExpression expL
        (accIdn, accDt) <- accessDataType accL

        -- Checking for TypeErrors
        guard (isValid accDt)
        guard (isValid expDt)
        unless (accDt == expDt) $ tellSError posn (InvalidAssignType accIdn accDt expDt)

    StReturn mayExpL -> flip (>>) (return True) . runMaybeT $ do
        (idn, retDt, _) <- lift currentFunction
        if isJust mayExpL
            then do
                expDt <- lift . typeCheckExpression $ fromJust mayExpL

                unlessGuard (not $ isVoid retDt) $ tellSError posn (ReturnInProcedure expDt idn)

                -- Checking for TypeError
                guard (isValid expDt)
                unlessGuard (retDt == expDt) $ tellSError posn (ReturnType retDt expDt idn)
            else unlessGuard (retDt == Void) $ tellSError posn (ReturnVoidInFunction retDt idn)

    StFunctionDef (Lex idn _) _ block -> do
        dt <- liftM (lexInfo . fromJust) $ getsSymbol idn returnType
        enterScope
        enterFunction idn dt
        ret <- typeCheckStatements block
        exitFunction
        exitScope

        -- When is a function and it wasn't properly returned
        unless (isVoid dt || ret) $ tellSError posn (NoReturn idn)
        return False

    StProcedureCall idnL expLs -> flip (>>) (return False) $ checkArguments idnL expLs False

    StRead accL -> flip (>>) (return False) . runMaybeT $ do
        (accIdn, accDt) <- accessDataType accL
        guard (isValid accDt)
        unless (isScalar accDt) $ tellSError posn (ReadNonReadable accDt accIdn)

    StPrint exprL -> flip (>>) (return False) . runMaybeT $ do
        dt <- lift $ typeCheckExpression exprL
        guard (isValid dt)
        unless (dt == String || isScalar dt) $ tellSError posn (PrintNonPrintable dt)

    StIf expL trueBlock falseBlock -> do
        expDt <- typeCheckExpression expL
        void . runMaybeT $ do
            guard (isValid expDt)
            when (expDt /= Bool) $ tellSError posn (ConditionDataType expDt)

        enterScope
        trueRet  <- typeCheckStatements trueBlock
        exitScope

        enterScope
        falseRet <- typeCheckStatements falseBlock
        exitScope

        return $ trueRet && falseRet

    StCase expL whnLs othrBlock -> do
        expDt <- typeCheckExpression expL
        unless (isScalar expDt) $ tellSError posn (CaseNonCaseable expDt)

        when (expDt == Bool) $ tellWarn (lexPosn expL) CaseOfBool

        -- For every when
        whnRets <- forM whnLs $ \(Lex (When whnExpLs whnBlock) whnP) -> do
            -- For every expression in said when
            forM_ whnExpLs $ typeCheckExpression >=> \whnExpDt -> runMaybeT $ do
                guard (isValid whnExpDt)
                guard (isValid expDt)
                unless (whnExpDt == expDt) $ tellSError whnP (CaseWhenDataType expDt whnExpDt)

            -- Check statements
            enterScope
            whnRet <- typeCheckStatements whnBlock
            exitScope
            return whnRet

        enterScope
        othrRet <- typeCheckStatements othrBlock
        exitScope

        return $ and whnRets && othrRet

    StLoop befBlock expL aftBlock -> do
        expDt <- typeCheckExpression expL
        void . runMaybeT $ do
            guard (isValid expDt)
            when (expDt /= Bool) $ tellSError posn (ConditionDataType expDt)

        enterScope
        befRet <- typeCheckStatements befBlock
        exitScope

        enterScope
        void $ typeCheckStatements aftBlock
        exitScope

        return befRet

    StFor _ expL block -> do
        expDt <- typeCheckExpression expL
        void . runMaybeT $ do
            guard (isValid expDt)
            when (expDt /= Range) $ tellSError posn (ForInDataType expDt)

        enterScope
        void $ typeCheckStatements block
        exitScope

        return False

    _ -> return False
    -- StVariableDeclaration
    -- StStructDefinition
    -- StBreak
    -- StContinue

typeCheckExpression :: Lexeme Expression -> TypeChecker DataType
typeCheckExpression (Lex exp posn) = case exp of

    LitInt    _ -> return Int
    LitFloat  _ -> return Float
    LitBool   _ -> return Bool
    LitChar   _ -> return Char
    LitString _ -> return String

    Variable accL -> liftM (fromMaybe TypeError) $ runMaybeT $ do
        (accIdn, accDt) <- accessDataType accL
        -- Mark used only when being evaluated for an expression
        markUsed accIdn
        return accDt

    FunctionCall idnL expLs -> liftM (fromMaybe TypeError) $ checkArguments idnL expLs True

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

checkArguments :: Lexeme Identifier -> Seq (Lexeme Expression) -> Bool -> TypeChecker (Maybe DataType)
checkArguments (Lex idn posn) args func = runMaybeT $ do
    maySymI <- getsSymbol idn (\sym -> (symbolCategory sym, lexInfo $ returnType sym, paramTypes sym))
    let (cat, dt, prms) = fromJust maySymI

    unlessGuard (isJust maySymI) $ tellSError posn (FunctionNotDefined idn)

    -- When is not a function
    unlessGuard (cat == CatFunction) $ tellSError posn (WrongCategory idn CatFunction cat)

    markUsed idn

    if func
        -- When is a procedure, and should be a function
        then when   (isVoid dt) $ tellSError posn (ProcedureInExpression idn)
        -- When is a function, and should be a procedure
        else unless (isVoid dt) $ tellSError posn (FunctionAsStatement idn)

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
    let (dt, cat) = fromJust maySymI

    unlessGuard (isJust maySymI) $ tellSError (lexPosn deepIdnL) (NotDefined deepIdn)

    -- Checking that it is a variable
    unlessGuard (cat == CatInfo) $ tellSError (lexPosn accL) (WrongCategory deepIdn CatInfo cat)

    dt' <- constructDataType deepIdnL deepZpp dt
    return (deepIdn, dt')

----------------------------------------

constructDataType :: Lexeme Identifier -> AccessZipper -> DataType -> MaybeT TypeChecker DataType
constructDataType idnL accZ dt = case defocusAccess <$> backAccess accZ of

    Just accL -> case lexInfo accL of

        ArrayAccess _ expL -> do
            unlessGuard (isArray dt) $ tellSError (lexPosn idnL) (AccessNonArray (lexInfo idnL) dt)

            expDt <- lift $ typeCheckExpression expL

            guard (isValid expDt)
            unlessGuard (expDt == Int) $ tellSError (lexPosn expL) (IndexDataType (lexInfo expL) expDt)

            constructDataType idnL (fromJust $ backAccess accZ) (arrayInnerDataType dt)

        StructAccess _ fldIdnL -> do
            unlessGuard (isStruct dt) $ tellSError (lexPosn idnL) (AccessNonStruct (lexInfo idnL) dt)

            -- This (fromJust . fromJust) may be dangerous (or not)
            tab <- liftM (fromJust . fromJust) $ getsSymbol (toIdentifier dt) fields

            -- Looks for the field in the struct's SymbolTable
            let mayFldDt = (lexInfo . dataType) <$> lookup (lexInfo fldIdnL) tab
            unlessGuard (isJust mayFldDt) $ tellSError (lexPosn fldIdnL) (StructNoField dt (lexInfo fldIdnL))

            constructDataType idnL (fromJust $ backAccess accZ) (fromJust mayFldDt)

        _ -> error "TypeChecker.constructDataType: unrecognized Access"

    Nothing -> return dt
