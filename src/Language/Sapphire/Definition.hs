module Language.Sapphire.Definition
    ( DefState(..)
    --, Definition
    --, buildDefinition
    --, runProgramDefinition
    , processDefinition
    ) where

import           Language.Sapphire.Error
import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad
import           Language.Sapphire.SymbolTable

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (liftM, unless, void, when)
import           Control.Monad.Reader          (asks)
import           Control.Monad.RWS             (RWS, runRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Trans.Maybe     (MaybeT, runMaybeT)
import           Control.Monad.Writer          (listen, tell)
import           Data.Foldable                 (all, foldl', foldlM, forM_,
                                                mapM_, maximum)
import           Data.Functor                  ((<$))
import qualified Data.Map.Strict               as Map (toList)
import           Data.Maybe                    (fromJust, isJust)
import           Data.Sequence                 (Seq, empty, index, null,
                                                singleton)
import           Data.Traversable              (forM)
import           Prelude                       hiding (all, filter, length,
                                                lookup, mapM_, maximum, null)

--------------------------------------------------------------------------------

type Definition = RWS SappReader SappWriter DefState

--------------------------------------------------------------------------------
-- State

data DefState = DefState
    { table   :: SymbolTable
    , stack   :: Stack Scope
    , scopeId :: ScopeNum
    , ast     :: Program
    , loopLvl :: Int
    }

----------------------------------------
-- Instances

instance SappState DefState where
    getTable   = table
    getStack   = stack
    getScopeId = scopeId
    getAst     = ast
    putTable   tab s = s { table   = tab }
    putStack   stk s = s { stack   = stk }
    putScopeId sc  s = s { scopeId = sc  }
    putAst     as  s = s { ast     = as  }

instance Show DefState where
    show = showSappState

----------------------------------------
-- Initial

initialState :: DefState
initialState = DefState
    { table     = emptyTable
    , stack     = topStack
    , scopeId   = topScopeNum
    , ast       = Program empty
    , loopLvl   = 0
    }

--------------------------------------------------------------------------------
-- Building the Monad

buildDefinition :: SappWriter -> Program -> Definition ()
buildDefinition w program@(Program block) = do
    initializeTable
    modify $ \s -> s { ast = program }
    tell w
    definitionStatements block

    (_, defW) <- listen (return ())

    when (null defW) $ do
        tab <- liftM allSymbols $ gets table
        -- First we define every DataType in the program,
        -- Then we get said DataTypes for the variables/parameters
        fixDataTypes tab
        --fixDataTypes $ filter ((==) CatType . symbolCategory . snd) tab
        --fixDataTypes $ filter ((/=) CatType . symbolCategory . snd) tab

----------------------------------------

initializeTable :: Definition ()
initializeTable = asks (Map.toList . types . arch) >>= \tuples -> forM_ tuples $
    \(dt, by) -> do
        let info = emptySymType
                { dataType = fillLex dt
                , langDef  = True
                , used     = True
                , width    = by
                }
        addSymbol (toIdentifier dt) info

--------------------------------------------------------------------------------
-- Using the Monad

processDefinition :: SappReader -> SappWriter -> Program -> (DefState, SappWriter)
processDefinition r w = runDefinition r . buildDefinition w

runDefinition :: SappReader -> Definition a -> (DefState, SappWriter)
runDefinition r = (\(_,s,w) -> (s,w)) . flip (flip runRWS r) initialState

--------------------------------------------------------------------------------
-- Monad handling

enterLoop :: Definition ()
enterLoop = modify (\s -> s { loopLvl = loopLvl s + 1 })

exitLoop :: Definition ()
exitLoop = modify (\s -> s { loopLvl = loopLvl s - 1 })

--------------------------------------------------------------------------------
-- Declaration

processDeclaration :: Lexeme Declaration -> Definition ()
processDeclaration (Lex (Declaration idnL dtL cat) dclP) = do
    stk <- gets stack
    let idn = lexInfo idnL
        info = emptySymInfo
            { dataType   = dtL
            , category   = cat
            , defPosn    = dclP
            , scopeStack = stk
            }
    maySymI <- getsSymbol idn $ \sym -> (scopeStack sym, defPosn sym, symbolCategory sym)
    case maySymI of
        Nothing -> addSymbol idn info
        Just (symStk, symDefP, symCat)
            | symCat == CatFunction -> tellSError dclP (FunctionAlreadyDefined idn symDefP)
            | symStk == stk         -> tellSError dclP (AlreadyDeclared idn symDefP)
            | otherwise             -> addSymbol idn info

--------------------------------------------------------------------------------
-- Statements

definitionStatements :: StBlock -> Definition ()
definitionStatements = mapM_ definitionStatement

definitionStatement :: Lexeme Statement -> Definition ()
definitionStatement (Lex st posn) = case st of

    StVariableDeclaration dclL -> processDeclaration dclL

    StStructDefinition dtL flds -> void $ runMaybeT $ do
        let idn = case lexInfo dtL of
                Record idnL -> lexInfo idnL
                Union  idnL -> lexInfo idnL

        current <- currentScope

        -- Can only define structures in the top scope
        unlessGuard (current == topScopeNum) $ tellSError posn TypeInInnerScope

        enterScope
        stk <- gets stack

        -- Fields SymbolTable
        fldsTab <- flip (flip foldlM emptyTable) flds $ \fldsTab (Lex fldIdn fldP, fldDtL) -> do
            let info = emptySymInfo
                    { dataType   = fldDtL
                    , category   = CatField
                    , used       = True
                    , defPosn    = fldP
                    , scopeStack = stk
                    }
                maySymI = lookup fldIdn fldsTab
            case maySymI of
                Nothing   -> return $ insert fldIdn info fldsTab
                Just symI -> tellSError fldP (AlreadyDeclared fldIdn (defPosn symI)) >> return fldsTab

        exitScope
        stk' <- gets stack

        let info = emptySymType
                { dataType   = dtL
                , fields     = Just fldsTab
                , defPosn    = lexPosn dtL
                , scopeStack = stk'
                }

        maySymI <- getsSymbol idn (langDef &&& defPosn)
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symLangD, symDefP)
                | symLangD  -> tellSError (lexPosn dtL) (TypeLanguageDefined idn)
                | otherwise -> tellSError (lexPosn dtL) (TypeAlreadyDefined idn symDefP)

    StFunctionDef idnL (Sign parms dtL) block -> do
        stk <- gets stack
        let idn = lexInfo idnL
            info = emptySymFunction
                { paramTypes = fmap (dclDataType . lexInfo) parms
                , returnType = dtL
                , body       = block
                , defPosn    = lexPosn idnL
                , scopeStack = stk
                }
        maySymI <- getsSymbol idn (\sym -> (scopeStack sym, defPosn sym, symbolCategory sym))
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symStk, symDefP, symCat)
                | symCat == CatFunction -> tellSError posn (FunctionAlreadyDefined idn symDefP)
                | symStk == stk         -> tellSError posn (AlreadyDeclared idn symDefP)
                | otherwise             -> addSymbol idn info

        enterScope
        mapM_ processDeclaration parms
        definitionStatements block
        exitScope

    StIf _ trueBlock falseBlock -> do
        enterScope
        definitionStatements trueBlock
        exitScope

        enterScope
        definitionStatements falseBlock
        exitScope

    StCase _ whnLs othrBlock -> do
        -- Definitions in each 'when'
        forM_ whnLs $ \(Lex (When _ wBlock) _) -> do
            enterScope
            definitionStatements wBlock
            exitScope
        -- Definitions in 'otherwise'
        enterScope
        definitionStatements othrBlock
        exitScope

    StLoop befBlock _ aftBlock -> do
        enterScope >> enterLoop
        definitionStatements befBlock
        exitScope

        enterScope
        definitionStatements aftBlock
        exitLoop >> exitScope


    StFor idnL _ block -> do
        let dcl = Declaration idnL (DataType (fillLex "Int") <$ idnL) CatVariable <$ idnL
        enterScope >> enterLoop
        processDeclaration dcl
        definitionStatements block
        exitLoop >> exitScope

    StBreak -> gets loopLvl >>= \lvl -> unless (lvl > 0) $ tellSError posn BreakOutsideLoop

    StContinue -> gets loopLvl >>= \lvl -> unless (lvl > 0) $ tellSError posn ContinueOutsideLoop

    _ -> return ()
    --StAssign
    --StReturn
    --StProcedureCall
    --StRead
    --StPrint


--------------------------------------------------------------------------------
-- DataType processing

fixDataTypes :: Seq (Identifier, Symbol) -> Definition ()
fixDataTypes syms = forM_ syms $ \(idn, sym) -> do
    currentTab <- gets table
    case symbolCategory sym of

        -- Variables and Parameters
        CatInfo -> void $ runMaybeT $ do
            (dtL', wdt) <- getUpdatedDataTypeWidth (scopeStack sym) (defPosn sym) (dataType sym)
            modifySymbolWithScope idn (scopeStack sym) (\sym' -> sym' { dataType = dtL', width = wdt })

        CatType -> do
            let symDtL  = dataType   sym
                symPosn = defPosn    sym
                symTab  = fields     sym
                symStk  = scopeStack sym

            -- We only need to check the user-defined types
            unless (langDef sym) $ do
                let strIdn = case lexInfo symDtL of
                        Record idnL -> lexInfo idnL
                        Union  idnL -> lexInfo idnL

                unless (isJust symTab) $ error "Definition.fixDataTypes: user-defined type has no fields SymbolTable"

                -- Fields
                symTab' <- forM (fromJust symTab) $ \fldSymSeq -> runMaybeT $ do
                    let fldSym   = index fldSymSeq 0
                        fldP     = defPosn fldSym
                        -- We need the field's data type identifier for errors
                        fldDtIdn = toIdentifier . lexInfo . fst . defocusDataType . deepDataType . focusDataType $ dataType fldSym
                        -- We need the field's data type position for errors
                        symDtP   = defPosn . fromJust $ lookupWithScope fldDtIdn symStk currentTab

                    (fldDtL, fldWdt) <- getUpdatedDataTypeWidth (scopeStack fldSym) fldP (dataType fldSym)

                    -- Error when defining a field with type of the same strucutre we are defining (recursively)
                    unlessGuard (strIdn /= fldDtIdn) $ tellSError fldP (RecursiveStruct strIdn)

                    -- Error when it uses a type defined afterwards
                    unlessGuard (symPosn > symDtP) $ tellSError fldP (TypeNotYetDefined strIdn fldDtIdn symDtP)

                    let fldSym' = fldSym { dataType = fldDtL, width = fldWdt }
                    return $ singleton fldSym'

                let (symTab'' , typeWidth) = case lexInfo symDtL of
                        Record _ -> foldRecordTable $ fmap fromJust symTab'
                        Union  _ -> widthUnionTable $ fmap fromJust symTab'
                when (all isJust symTab') $
                    modifySymbolWithScope idn symStk (\sym' -> sym' { fields = Just symTab'', width = typeWidth })
                where
                    widthUnionTable :: SymbolTable -> (SymbolTable, Width)
                    widthUnionTable symTab = (symTab, maximum . fmap (width . snd) $ allSymbols symTab)
                    foldRecordTable :: SymbolTable -> (SymbolTable, Width)
                    foldRecordTable symTab = foldl' recordField (symTab, 0) $ allSymbols symTab
                    recordField :: (SymbolTable, Width) -> (Identifier, Symbol) -> (SymbolTable, Width)
                    recordField (tab, accum) (fldIdn, fldSym) = (tab', accum + width fldSym)
                        where
                            tab'   = update fldIdn (\fldSym' -> fldSym' { offset = accum }) tab

        CatFunction -> do
            let symStk  = scopeStack sym
                symPosn = defPosn sym
            mayRetDtL    <- runMaybeT $ getUpdatedDataType symStk symPosn (returnType sym)
            mayParamDtLs <- forM (paramTypes sym) $ \paramDtL ->
                runMaybeT $ getUpdatedDataType symStk symPosn paramDtL

            when (all isJust mayParamDtLs && isJust mayRetDtL) $ do
                let retDtL    = fromJust mayRetDtL
                    paramDtLs = fmap fromJust mayParamDtLs
                    -- Procedures are auotmatically 'returned'
                    symRet    = isVoid $ lexInfo retDtL
                modifySymbolWithScope idn (scopeStack sym) (\sym' -> sym' { returnType = retDtL, paramTypes = paramDtLs, returned = symRet })

----------------------------------------

getUpdatedDataType :: Stack Scope -> Position -> Lexeme DataType -> MaybeT Definition (Lexeme DataType)
getUpdatedDataType stk posn dtL = liftM fst $ getUpdatedDataTypeWidth stk posn dtL

getUpdatedDataTypeWidth :: Stack Scope -> Position -> Lexeme DataType -> MaybeT Definition (Lexeme DataType, Width)
getUpdatedDataTypeWidth stk posn dtL = if isVoid $ lexInfo dtL then return (dtL, 0)
    else do
        maySymI <- getsSymbolWithStack deepDtIdn stk (lexInfo . dataType &&& width)
        let (symDt, wdt) = fromJust maySymI
            deepDtL'     = symDt <$ deepDtIdnL
            dtL'         = fst . defocusDataType . topDataType $ putDataType deepDtL' deepZpp

        -- Error when the DataType was not found in the SymbolTable
        unlessGuard (isJust maySymI) $ tellSError posn (UndefinedType deepDtIdn)

        -- Mark the DataType as used
        markUsed deepDtIdn

        return (dtL', wdt * dim)
    where
        deepZpp             = deepDataType $ focusDataType dtL
        (deepDtL, dim)      = defocusDataType deepZpp
        DataType deepDtIdnL = lexInfo deepDtL
        deepDtIdn           = lexInfo deepDtIdnL
