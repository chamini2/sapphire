module Definition
    ( DefState(..)
    , processDefinition

    , Definition
    , definitionProgram
    , runProgramDefinition
    --, processDefinition
    )where

import           Error
import           Program
import           SymbolTable
import           RWS

import           Control.Arrow             ((&&&))
import           Control.Monad.RWS         hiding (forM, forM_, mapM, mapM_)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Foldable             as DF (all, any, forM_, mapM_)
import           Data.Functor              ((<$), (<$>))
import qualified Data.Map                  as DM (toList)
import           Data.Maybe                (fromJust, isJust, isNothing)
import           Data.Sequence             as DS (Seq, empty, filter)
import           Data.Traversable          (forM)
import           Prelude                   as P hiding (all, any, mapM_, filter)

--------------------------------------------------------------------------------

type Definition = RWS SappReader SappWriter DefState

--------------------------------------------------------------------------------
-- State

data DefState = DefState
    { table   :: SymbolTable
    , stack   :: Stack Scope
    , scopeId :: ScopeNum
    , ast     :: Program
    }

instance Show DefState where
    show (DefState t s c a) = showT ++ showS ++ showC ++ showA
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
    , ast       = Program DS.empty
    }

--------------------------------------------------------------------------------
-- Building the Monad

definitionProgram :: SappWriter -> Program -> Definition ()
definitionProgram w program@(Program block) = do
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
initializeTable = asks arch >>=
    \arc -> forM_ (DM.toList $ widths arc) $ \(dt, wth) -> do
        let info = emptySymType
                { dataType = fillLex dt
                , langDef  = True
                , used     = True
                , width    = wth
                }
        addSymbol (show dt) info

--------------------------------------------------------------------------------
-- Using the Monad

processDefinition :: SappWriter -> Program -> (DefState, SappWriter)
processDefinition w = runProgramDefinition . definitionProgram w

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

----------------------------------------
-- Scope

enterScope :: Definition ()
enterScope = do
    currentId <- gets scopeId
    let scope = Scope { serial = currentId + 1 }
    modify $ \s -> s { stack = push scope (stack s), scopeId = currentId + 1 }

exitScope :: Definition ()
exitScope = modify $ \s -> s { stack = pop $ stack s }

currentScope :: Definition ScopeNum
currentScope = gets (serial . top . stack)

----------------------------------------
-- Symbol

addSymbol :: Identifier -> Symbol -> Definition ()
addSymbol idn info = modify $ \s -> s { table = insert idn info (table s) }

--getSymbol :: Identifier -> Definition (Maybe Symbol)
--getSymbol = flip getsSymbol id

getsSymbol :: Identifier -> (Symbol -> a) -> Definition (Maybe a)
getsSymbol idn f = do
    (tab, stk) <- gets (table &&& stack)
    return $ f <$> lookupWithScope idn stk tab -- f <$> == maybe Nothing (Just . f)

--modifySymbol :: Identifier -> (Symbol -> Symbol) -> Definition ()
--modifySymbol idn f = do
--    maySymI <- getsSymbol idn scopeNum
--    case maySymI of
--        Nothing -> return ()
--        Just symScopeN -> modifySymbolWithScope idn f symScopeN

modifySymbolWithScopeNStack :: Identifier -> (Symbol -> Symbol) -> ScopeNum -> Stack Scope -> Definition ()
modifySymbolWithScopeNStack idn f scope stk = do
    tab <- gets table
    let exists = isJust $ lookupWithScope idn stk tab
    when exists $ modify $ \s -> s { table = updateWithScope idn f scope tab }

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
    maySymI <- getsSymbol idn $ \s -> (scopeNum s, defPosn s, symbolCategory s)
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
definitionStatement (Lex st stP) = case st of

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
        maySymI <- getsSymbol idn (\s -> (scopeNum s, defPosn s, symbolCategory s))
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symScopeN, symDefP, symCat)
                | symCat == CatFunction -> tellSError stP (FunctionRedefinition idn symDefP)
                | symScopeN == current  -> tellSError stP (AlreadyDeclared idn symDefP)
                | otherwise             -> addSymbol idn info

        enterScope
        mapM_ processDeclaration parms

        enterScope
        definitionStatements block
        exitScope

        exitScope

    StIf _ success failure -> do
        enterScope
        definitionStatements success
        exitScope

        enterScope
        definitionStatements failure
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

    StLoop rep _ block -> do
        enterScope
        definitionStatements rep
        exitScope

        enterScope
        definitionStatements block
        exitScope

    StFor idnL _ block -> do
        let dcl = Declaration idnL (Int <$ idnL) CatVariable <$ idnL
        enterScope
        processDeclaration dcl
        definitionStatements block
        exitScope

    _ -> return ()

--------------------------------------------------------------------------------
-- DataType processing

fixDataTypes :: Seq (Identifier, Symbol) -> Definition ()
fixDataTypes syms = forM_ syms $ \(idn, sym) -> do
    currentTab <- gets table
    case symbolCategory sym of

        -- Variables and Parameters
        CatInfo     -> void $ runMaybeT $ do
            newDtL <- getUpdatedDataType (scopeStack sym) (defPosn sym) (dataType sym)
            lift $ modifySymbolWithScopeNStack idn (\s -> s { dataType = newDtL}) (scopeNum sym) (scopeStack sym)

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
                    unless (strIdn /= fldDtIdn) . lift $ tellSError (lexPosn fldIdnL) (RecursiveStruct strIdn (lexInfo fldIdnL))
                    guard  (strIdn /= fldDtIdn)

                    -- Error when it uses a type defined afterwards
                    unless (symPosn > symDtP) . lift $ tellSError (lexPosn fldIdnL) (TypeNotYetDefined strIdn (lexInfo fldIdnL) fldDtIdn symDtP)
                    guard  (symPosn > symDtP)

                    return (fldIdnL, newFldDtL)

                unless (any isNothing newFlds) $ do
                    let newDtL = case lexInfo symDtL of
                            Record strIdnL _ -> Record strIdnL (fmap fromJust newFlds) <$ symDtL
                            Union  strIdnL _ -> Union  strIdnL (fmap fromJust newFlds) <$ symDtL
                    modifySymbolWithScopeNStack idn (\s -> s { dataType = newDtL }) (scopeNum sym) (scopeStack sym)

        CatFunction -> do
            let symStk  = scopeStack sym
                symPosn = defPosn sym
            mayRetDtL    <- runMaybeT $ getUpdatedDataType symStk symPosn (returnType sym)
            mayParamDtLs <- forM (paramTypes sym) $ \paramDtL ->
                runMaybeT $ getUpdatedDataType symStk symPosn paramDtL

            when (all isJust mayParamDtLs && isJust mayRetDtL) $ do
                let retDtL    = fromJust mayRetDtL
                    paramDtLs = fmap fromJust mayParamDtLs
                modifySymbolWithScopeNStack idn (\s -> s { returnType = retDtL, paramTypes = paramDtLs}) (scopeNum sym) (scopeStack sym)

----------------------------------------

getUpdatedDataType :: Stack Scope -> Position -> Lexeme DataType -> MaybeT Definition (Lexeme DataType)
getUpdatedDataType stk posn dtL = if lexInfo dtL == Void then return dtL
    else do
        tab <- gets table
        let maySym = lookupWithScope deepDtIdn stk tab
            newDeepDtL = lexInfo (dataType (fromJust maySym)) <$ deepDtIdnL
            newDtL     = defocusDataType . topDataType $ putDataType newDeepDtL deepZpp

        -- Error when the DataType was not found in the SymbolTable
        unless (isJust maySym) . lift $ tellSError posn (UndefinedType deepDtIdn)
        guard  (isJust maySym)

        return newDtL
    where
        deepZpp             = deepDataType $ focusDataType dtL
        DataType deepDtIdnL = lexInfo $ defocusDataType deepZpp
        deepDtIdn           = lexInfo deepDtIdnL
