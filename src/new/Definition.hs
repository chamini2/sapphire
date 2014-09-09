module Definition where

import           Error
import           Language
import           Scope
import           Stack
import           SymbolTable
import           Position
import           Lexeme

import           Control.Arrow          ((&&&))
--import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS      hiding (forM, forM_, mapM, mapM_)
import           Data.Foldable          as DF (mapM_, forM_)
--import           Data.Foldable          as DF (all, and, concatMap, elem, find, foldl, foldlM, foldr, foldr1, forM_, mapM_, notElem, sum, toList)
import           Data.Function          (on)
import           Data.Functor           ((<$>))
--import           Data.Functor           ((<$), (<$>))
--import           Data.List              (intercalate)
import qualified Data.Map               as DM (Map, fromList, toList)
--import qualified Data.Map               as DM (Map, empty, fromList, insertLookupWithKey, lookup)
--import           Data.Maybe             (fromJust, isJust, isNothing)
import           Data.Sequence          as DS (Seq, singleton, empty)
--import           Data.Sequence          as DS (Seq, ViewL ((:<), EmptyL), empty, filter, fromList, index, length, null, singleton, sortBy, viewl, zip, zipWith, (<|), (><), (|>))
--import           Data.Traversable       as DT (forM, mapM)
--import           GHC.Generics
import           Prelude                as P hiding (mapM_)
--import           Prelude                as P hiding (all, and, concatMap, elem, filter, foldl, foldr, foldr1, length, lookup, mapM, mapM_, notElem, null, sum, zip, zipWith)

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
    { table     :: SymbolTable
    , stack     :: Stack Scope
    , scopeId   :: ScopeNum
    , ast       :: Program
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

addSymbol :: Identifier -> SymbolInfo -> Definition ()
addSymbol idn info = modify $ \s -> s { table = insert idn info (table s) }

--getSymbolInfo :: Identifier -> Checker (Maybe a)
--getSymbolInfo idn = getsSymbolInfo idn id

getsSymbolInfo :: Identifier -> (SymbolInfo -> a) -> Definition (Maybe a)
getsSymbolInfo idn f = do
    (tab, stck) <- gets (table &&& stack)
    return $ f <$> lookupWithScope idn stck tab -- f <$> == maybe Nothing (Just . f)

--------------------------------------------------------------------------------

processDeclaration :: Lexeme Declaration -> Definition Bool
processDeclaration (Lex (Declaration idnL dtL cat) dclP) = do
    current <- currentScope
    let idn = lexInfo idnL
        info = emptySymbolInfo
            { dataType = lexInfo dtL
            , category = cat
            , scopeNum = current
            , defPosn  = dclP
            }
    maySymI <- getsSymbolInfo idn (\s -> (scopeNum s, defPosn s, category s))
    case maySymI of
        Nothing -> addSymbol idn info >> return True
        Just (symScopeN, symDefP, symCat)
            | symCat == CatFunction -> tellSError dclP (FunctionRedefinition idn symDefP) >> return False
            | symScopeN == current  -> tellSError dclP (AlreadyDeclared idn symDefP)      >> return False
            | otherwise             -> addSymbol idn info >> return True

--------------------------------------------------------------------------------

initializeTable :: Architecture -> Definition ()
initializeTable arc = forM_ (DM.toList $ widths arc) $ \(dt, wth) -> do
        let info = emptySymbolInfo
                { dataType = Type
                , category = CatLanguage
                , initial  = True
                , used     = True
                , value    = Just $ ValType dt wth
                }
        addSymbol (show dt) info

definitionProgram :: Seq Error -> Program -> Definition ()
definitionProgram lexErrors program@(Program block) = do
    arc <- asks arch
    initializeTable arc
    modify $ \s -> s { ast = program }
    tell lexErrors
    definitionStatements block

definitionStatements :: StBlock -> Definition ()
definitionStatements = mapM_ definitionStatement

definitionStatement :: Lexeme Statement -> Definition ()
definitionStatement stL = case lexInfo stL of

    --StAssign accL expL ->

    StVariableDeclaration dclL -> void $ processDeclaration dclL

    StStructDefinition (Lex dt dtP) -> do
        current <- currentScope
        let idn = case dt of
                Record idnL _ -> lexInfo idnL
                Union  idnL _ -> lexInfo idnL
            info = emptySymbolInfo
                { dataType = Type
                , category = CatUserDef
                , value    = Just $ ValType dt (-1)
                , scopeNum = current
                , defPosn  = dtP
                , initial  = True
                }
        maySymI <- getsSymbolInfo idn (scopeNum &&& defPosn)
        case maySymI of
            Nothing -> addSymbol idn info
            Just (symScopeN, symDefP)
                | symScopeN == -1 -> tellSError dtP (TypeIsLanguageDefined idn)
                | otherwise       -> tellSError dtP (TypeAlreadyDefined idn symDefP)

    StFunctionDef dclL sig block -> do
        added <- processDeclaration dclL

        when added $ do
            enterScope
            mapM_ processDeclaration sig
            definitionStatements block
            exitScope

    --StProcedureCall idnL expLs ->

    --StRead accL -> return ()

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

    --StLoop rep cndL block ->

    --StFor idnL expL block ->

    _ -> return ()
    --StNoop -> return ()
    --StReturn expL -> return ()
    --StPrint expL -> return ()
    --StBreak -> return ()
    --StContinue -> return ()

