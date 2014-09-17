module Definition
    ( DefState(..)
    --, Definition
    --, buildDefinition
    --, runProgramDefinition
    , processDefinition
    ) where

import           Error
import           Program
import           SappMonad
import           SymbolTable

import           Control.Arrow             ((&&&))
import           Control.Monad             (liftM, void, unless, when)
import           Control.Monad.State       (gets, modify)
import           Control.Monad.Reader      (asks)
import           Control.Monad.Writer      (tell)
import           Control.Monad.RWS         (RWS, runRWS)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Foldable             as DF (all, forM_, mapM_)
import           Data.Functor              ((<$), (<$>))
import qualified Data.Map                  as DM (toList)
import           Data.Maybe                (fromJust, isJust)
import           Data.Sequence             as DS (Seq, empty, filter)
import           Data.Traversable          (forM)
import           Prelude                   as P hiding (all, filter, mapM_)

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
    show (DefState t s c a _) = showT ++ showS ++ showC ++ showA
        where
            showT = show t ++ "\n"
            showS = "Scope Stack:\n"  ++ show s ++ "\n"
            showC = "Scope Number:\t" ++ show c ++ "\n"
            showA = show a ++ "\n"

----------------------------------------
-- Initial

initialState :: DefState
initialState = DefState
    { table     = emptyTable
    , stack     = initialStack
    , scopeId   = 0
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

    tab <- liftM accessible $ gets table
    -- First we define every DataType in the program,
    -- Then we get said DataTypes for the variables/parameters
    fixDataTypes $ filter ((==) CatType . symbolCategory . snd) tab
    fixDataTypes $ filter ((/=) CatType . symbolCategory . snd) tab

----------------------------------------

initializeTable :: Definition ()
initializeTable = asks (DM.toList . types . arch) >>= \tuples -> forM_ tuples $
    \(dt, byt) -> do
        let info = emptySymType
                { dataType = fillLex dt
                , langDef  = True
                , used     = True
                , bytes    = byt
                }
        addSymbol (show dt) info

--------------------------------------------------------------------------------
-- Using the Monad

processDefinition :: SappWriter -> Program -> (DefState, SappWriter)
processDefinition w = runProgramDefinition . buildDefinition w

runProgramDefinition :: Definition a -> (DefState, SappWriter)
runProgramDefinition = (\(_,s,w) -> (s,w)) . runDefinition

runDefinitionWithReader :: SappReader -> Definition a -> (a, DefState, SappWriter)
runDefinitionWithReader r = flip (flip runRWS r) initialState

runDefinition :: Definition a -> (a, DefState, SappWriter)
runDefinition = runDefinitionWithReader initialReader

getDefinition :: Definition a -> a
getDefinition = (\(v,_,_) -> v) . runDefinition

getWriter :: Definition a -> SappWriter
getWriter = (\(_,_,w) -> w) . runDefinition

getState :: Definition a -> DefState
getState = (\(_,s,_) -> s) . runDefinition

--------------------------------------------------------------------------------
-- Monad handling

enterLoop :: Definition ()
enterLoop = modify (\s -> s { loopLvl = loopLvl s + 1 })

exitLoop :: Definition ()
exitLoop = modify (\s -> s { loopLvl = loopLvl s - 1 })

--------------------------------------------------------------------------------
-- Declaration

processDeclaration :: Lexeme Declaration -> Definition Bool
processDeclaration (Lex (Declaration idnL dtL cat) dclP) = do
    current <- currentScope
    stk     <- gets stack
    let idn = lexInfo idnL
        info = emptySymInfo
            { dataType   = dtL
            , category   = cat
            , scopeNum   = current
            , defPosn    = dclP
            , scopeStack = stk
            }
    maySymI <- getsSymbol idn $ \sym -> (scopeNum sym, defPosn sym, symbolCategory sym)
    case maySymI of
        Nothing -> addSymbol idn info >> return True
        Just (symScopeN, symDefP, symCat)
            | symCat == CatFunction -> tellSError dclP (FunctionRedefinition idn symDefP) >> return False
            | symScopeN == current  -> tellSError dclP (AlreadyDeclared idn symDefP)      >> return False
            | otherwise             -> addSymbol idn info >> return True

--------------------------------------------------------------------------------
-- Statements

definitionStatements :: StBlock -> Definition ()
definitionStatements = mapM_ definitionStatement

definitionStatement :: Lexeme Statement -> Definition ()
definitionStatement (Lex st posn) = case st of

    StVariableDeclaration dclL -> void $ processDeclaration dclL

    StStructDefinition dtL -> do
        current <- currentScope
        stk     <- gets stack
        let idn = case lexInfo dtL of
                Record idnL _ -> lexInfo idnL
                Union  idnL _ -> lexInfo idnL
            info = emptySymType
                { dataType   = dtL
                , defPosn    = lexPosn dtL
                , scopeNum   = current
                , scopeStack = stk
                }
        maySymI <- getsSymbol idn (langDef &&& defPosn)
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symLangD, symDefP)
                | symLangD  -> tellSError (lexPosn dtL) (TypeIsLanguageDefined idn)
                | otherwise -> tellSError (lexPosn dtL) (TypeAlreadyDefined idn symDefP)

    StFunctionDef idnL (Sign parms dtL) block -> do
        current <- currentScope
        stk     <- gets stack
        let idn = lexInfo idnL
            info = emptySymFunction
                { paramTypes = dclDataType . lexInfo <$> parms      -- fmap (dclDataType . lexInfo) parms
                , returnType = dtL
                , body       = block
                , defPosn    = lexPosn idnL
                , scopeNum   = current
                , scopeStack = stk
                }
        maySymI <- getsSymbol idn (\sym -> (scopeNum sym, defPosn sym, symbolCategory sym))
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symScopeN, symDefP, symCat)
                | symCat == CatFunction -> tellSError posn (FunctionRedefinition idn symDefP)
                | symScopeN == current  -> tellSError posn (AlreadyDeclared idn symDefP)
                | otherwise             -> addSymbol idn info

        enterScope
        mapM_ processDeclaration parms

        enterScope
        definitionStatements block
        exitScope

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

    StContinue -> gets loopLvl >>= \lvl -> unless (lvl > 0) $ tellSError posn BreakOutsideLoop

    _ -> return ()
    --StNoop
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
        CatInfo     -> void $ runMaybeT $ do
            newDtL <- getUpdatedDataType (scopeStack sym) (defPosn sym) (dataType sym)
            modifySymbolWithScopeNStack idn (scopeNum sym) (scopeStack sym) (\sym' -> sym' { dataType = newDtL})

        CatType     -> do
            let symDtL  = dataType sym
                symPosn = defPosn  sym
            -- We only need to check the user-defined types
            unless (langDef sym) $ do
                let (strIdn, flds) = case lexInfo symDtL of
                        Record idnL fields -> (lexInfo idnL, fields)
                        Union  idnL fields -> (lexInfo idnL, fields)
                -- Fields
                newFlds <- forM flds $ \(fldIdnL, fldDtL) -> runMaybeT $ do
                    -- We need the field's data type identifier for errors
                    let fldDtIdn = (\(DataType idnL) -> lexInfo idnL) . lexInfo . defocusDataType . deepDataType $ focusDataType fldDtL
                        symDtP   = defPosn . fromJust $ lookupWithScope fldDtIdn (scopeStack sym) currentTab

                    newFldDtL <- getUpdatedDataType (scopeStack sym) (lexPosn fldIdnL) fldDtL

                    -- Error when defining a field with type of the same strucutre we are defining (recursively)
                    unlessGuard (strIdn /= fldDtIdn) $ tellSError (lexPosn fldIdnL) (RecursiveStruct strIdn (lexInfo fldIdnL))

                    -- Error when it uses a type defined afterwards
                    unlessGuard (symPosn > symDtP) $ tellSError (lexPosn fldIdnL) (TypeNotYetDefined strIdn (lexInfo fldIdnL) fldDtIdn symDtP)

                    return (fldIdnL, newFldDtL)

                when (all isJust newFlds) $ do
                    let newDtL = case lexInfo symDtL of
                            Record strIdnL _ -> Record strIdnL (fmap fromJust newFlds) <$ symDtL
                            Union  strIdnL _ -> Union  strIdnL (fmap fromJust newFlds) <$ symDtL
                    modifySymbolWithScopeNStack idn (scopeNum sym) (scopeStack sym) (\sym' -> sym' { dataType = newDtL })

        CatFunction -> do
            let symStk  = scopeStack sym
                symPosn = defPosn sym
            mayRetDtL    <- runMaybeT $ getUpdatedDataType symStk symPosn (returnType sym)
            mayParamDtLs <- forM (paramTypes sym) $ \paramDtL ->
                runMaybeT $ getUpdatedDataType symStk symPosn paramDtL

            when (all isJust mayParamDtLs && isJust mayRetDtL) $ do
                let retDtL    = fromJust mayRetDtL
                    paramDtLs = fmap fromJust mayParamDtLs
                modifySymbolWithScopeNStack idn (scopeNum sym) (scopeStack sym) (\sym' -> sym' { returnType = retDtL, paramTypes = paramDtLs})

----------------------------------------

getUpdatedDataType :: Stack Scope -> Position -> Lexeme DataType -> MaybeT Definition (Lexeme DataType)
getUpdatedDataType stk posn dtL = if lexInfo dtL == Void then return dtL
    else do
        mayDt <- getsSymbolWithStack deepDtIdn stk (lexInfo . dataType)
        let newDeepDtL = fromJust mayDt <$ deepDtIdnL
            newDtL     = defocusDataType . topDataType $ putDataType newDeepDtL deepZpp

        -- Error when the DataType was not found in the SymbolTable
        unlessGuard (isJust mayDt) $ tellSError posn (UndefinedType deepDtIdn)

        return newDtL
    where
        deepZpp             = deepDataType $ focusDataType dtL
        DataType deepDtIdnL = lexInfo $ defocusDataType deepZpp
        deepDtIdn           = lexInfo deepDtIdnL
