module Definition where

import           Error
import           Program
import           SymbolTable

import           Control.Arrow     ((&&&))
import           Control.Monad.RWS hiding (forM, forM_, mapM, mapM_)
import           Data.Foldable     as DF (forM_, mapM_)
import           Data.Function     (on)
import           Data.Functor      ((<$), (<$>))
import qualified Data.Map          as DM (Map, fromList, toList)
import           Data.Sequence     as DS (Seq, empty, singleton)
import           Prelude           as P hiding (mapM_)

--------------------------------------------------------------------------------

type Definition a = RWS SapphireReader DefWriter DefState a

--------------------------------------------------------------------------------
-- Reader

data SapphireReader = SapphireReader
    { flags :: Seq Flag
    , arch  :: Architecture
    }

data Flag = OutputFile FilePath | SupressWarnings | AllWarnings
    deriving (Eq)

data Architecture = Arch
    { archName :: String
    , widths   :: DM.Map DataType Width
    } deriving (Show)

instance Eq Architecture where
    (==) = (==) `on` archName

--------------------------------------------------------------------------------
-- Writer

type DefWriter = Seq Error

--------------------------------------------------------------------------------
-- State

data DefState = DefState
    { table   :: SymbolTable
    , stack   :: Stack Scope
    , scopeId :: ScopeNum
    , ast     :: Program
    --, loopLvl   :: NestedLevel
    --, funcStack :: Stack (DataType, Lexeme Identifier, ScopeNum)
    --, offsStack :: Stack Offset
    }

instance Show DefState where
    show (DefState t s c a) = showT ++ showS ++ showC ++ showA
        where
            showT = show t ++ "\n"
            showS = "Scope Stack:\n"  ++ show s ++ "\n"
            showC = "Scope Number:\t" ++ show c ++ "\n"
            showA = show a ++ "\n"

type NestedLevel = Int

--------------------------------------------------------------------------------
-- Monad functions

tellLError :: Position -> LexerError -> Definition ()
tellLError posn err = tell (singleton $ LError posn err)

tellPError :: Position -> ParseError -> Definition ()
tellPError posn err = tell (singleton $ PError posn err)

tellSError :: Position -> StaticError -> Definition ()
tellSError posn err = tell (singleton $ SError posn err)

tellWarn :: Position -> Warning -> Definition ()
tellWarn posn err = tell (singleton $ Warn posn err)

----------------------------------------
-- Initials

initialReader :: SapphireReader
initialReader = SapphireReader
    { flags  = empty
    , arch   = defaultArchitecture
    }

initialWriter :: DefWriter
initialWriter = empty

initialState :: DefState
initialState = DefState
    { table     = emptyTable
    , stack     = initialStack
    , scopeId   = 0
    , ast       = Program DS.empty
    --, loopLvl   = 0
    --, funcStack = singletonStack (Void, Lex "sapphire" (0,0), -1)
    --, offsStack = singletonStack 0
    }

defaultArchitecture :: Architecture
defaultArchitecture = Arch
    { archName = "mips"
    , widths = DM.fromList
        [ (Int     , 32)
        , (Float   , 32)
        , (Char    , 8)
        , (Bool    , 8)
        {-, (Pointer , 32)-}
        ]
    }

--------------------------------------------------------------------------------
-- Using the Monad

runProgramDefinition :: Definition a -> (DefState, DefWriter)
runProgramDefinition = (\(_,s,w) -> (s,w)) . runDefinition

runDefinitionWithReader :: SapphireReader -> Definition a -> (a, DefState, DefWriter)
runDefinitionWithReader r = flip (flip runRWS r) initialState

runDefinition :: Definition a -> (a, DefState, DefWriter)
runDefinition = flip (flip runRWS initialReader) initialState

getDefinition :: Definition a -> a
getDefinition = (\(v,_,_) -> v) . runDefinition

getWriter :: Definition a -> DefWriter
getWriter = (\(_,_,w) -> w) . runDefinition

getState :: Definition a -> DefState
getState = (\(_,s,_) -> s) . runDefinition

--------------------------------------------------------------------------------
-- Monad handling

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

addSymbol :: Identifier -> Symbol -> Definition ()
addSymbol idn info = modify $ \s -> s { table = insert idn info (table s) }

getSymbol :: Identifier -> Definition (Maybe Symbol)
getSymbol = flip getsSymbol id

getsSymbol :: Identifier -> (Symbol -> a) -> Definition (Maybe a)
getsSymbol idn f = do
    (tab, stk) <- gets (table &&& stack)
    return $ f <$> lookupWithScope idn stk tab -- f <$> == maybe Nothing (Just . f)

----------------------------------------

--modifySymbol :: Identifier -> (Symbol -> Symbol) -> Definition ()
--modifySymbol idn f = do
--    maySymI <- getsSymbol idn scopeNum
--    -- forM_ maySymI (modifySymbolWithScope idn f)
--    case maySymI of
--        Nothing -> return ()
--        Just symScopeN -> modifySymbolWithScope idn f symScopeN

--modifySymbolWithScope :: Identifier -> (Symbol -> Symbol) -> ScopeNum -> Definition ()
--modifySymbolWithScope idn f scope = do
--    tab <- gets table
--    maySymI <- getSymbol idn
--    case maySymI of
--        Just _  -> modify (\s -> s { table = updateWithScope idn scope f tab })
--        Nothing -> return ()

--------------------------------------------------------------------------------

processDeclaration :: Lexeme Declaration -> Definition Bool
processDeclaration (Lex (Declaration idnL dtL cat) dclP) = do
    current <- currentScope
    let idn = lexInfo idnL
        info = emptySymInfo
            { dataType = lexInfo dtL
            , category = cat
            , scopeNum = current
            , defPosn  = dclP
            }
    maySymI <- getsSymbol idn (\s -> (scopeNum s, defPosn s, symbolCategory s))
    case maySymI of
        Nothing -> addSymbol idn info >> return True
        Just (symScopeN, symDefP, symCat)
            | symCat == CatFunction -> tellSError dclP (FunctionRedefinition idn symDefP) >> return False
            | symScopeN == current  -> tellSError dclP (AlreadyDeclared idn symDefP)      >> return False
            | otherwise             -> addSymbol idn info >> return True

--------------------------------------------------------------------------------

initializeTable :: Definition ()
initializeTable = asks arch >>= \arc -> forM_ (DM.toList $ widths arc) $ \(dt, wth) -> do
        let info = emptySymType
                { dataType = dt
                , langDef  = True
                , used     = True
                , width    = wth
                }
        addSymbol (show dt) info

definitionProgram :: Seq Error -> Program -> Definition ()
definitionProgram lexErrors program@(Program block) = do
    initializeTable
    modify $ \s -> s { ast = program }
    tell lexErrors
    definitionStatements block

definitionStatements :: StBlock -> Definition ()
definitionStatements = mapM_ definitionStatement

definitionStatement :: Lexeme Statement -> Definition ()
definitionStatement (Lex st stP) = case st of

    StVariableDeclaration dclL -> void $ processDeclaration dclL

    StStructDefinition (Lex dt dtP) -> do
        current <- currentScope
        let idn = case dt of
                Record idnL _ -> lexInfo idnL
                Union  idnL _ -> lexInfo idnL
            info = emptySymType
                { dataType = dt
                , defPosn  = dtP
                , scopeNum = current
                }
        maySymI <- getsSymbol idn (langDef &&& defPosn)
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symLangD, symDefP)
                | symLangD  -> tellSError dtP (TypeIsLanguageDefined idn)
                | otherwise -> tellSError dtP (TypeAlreadyDefined idn symDefP)

    StFunctionDef idnL (Sign parms dtL) block -> do
        current <- currentScope
        let idn = lexInfo idnL
            info = emptySymFunction
                { paramTypes = dclDataType . lexInfo <$> parms      -- fmap (dclDataType . lexInfo) parms
                , returnType = lexInfo dtL
                , body       = block
                , defPosn    = lexPosn idnL
                , scopeNum   = current
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
        definitionStatements block
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
