module SizeOffset
    ( SizeState

    , SizeOffset
    , processSizeOffset
    ) where

--import           Error
import           Program
import           SappMonad
import           SymbolTable

import           Control.Arrow             ((&&&))
import           Control.Monad             (liftM)
--import           Control.Monad             (liftM, void, unless, when)
import           Control.Monad.State       (gets, modify)
--import           Control.Monad.Reader      (asks)
import           Control.Monad.Writer      (tell)
import           Control.Monad.RWS         (RWS, runRWS)
--import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Foldable             as DF (forM_, mapM_)
--import           Data.Foldable             as DF (all, forM_, mapM_)
--import           Data.Functor              ((<$), (<$>))
--import qualified Data.Map                  as Map (toList)
import           Data.Maybe                (fromJust)
--import           Data.Maybe                (fromJust, isJust)
import           Data.Sequence             as DS (Seq, empty)
--import           Data.Sequence             as DS (Seq, empty, filter)
--import           Data.Traversable          (forM)
import           Prelude                   hiding (mapM_)
--import           Prelude                   as P hiding (all, filter, mapM_)

--------------------------------------------------------------------------------

type SizeOffset = RWS SappReader SappWriter SizeState

--------------------------------------------------------------------------------
-- State

data SizeState = SizeState
    { table    :: SymbolTable
    , stack    :: Stack Scope
    , scopeId  :: ScopeNum
    , ast      :: Program
    , offStack :: Stack Offset
    }

----------------------------------------
-- Instances

instance SappState SizeState where
    getTable   = table
    getStack   = stack
    getScopeId = scopeId
    getAst     = ast
    putTable   tab s = s { table   = tab }
    putStack   stk s = s { stack   = stk }
    putScopeId sc  s = s { scopeId = sc  }
    putAst     as  s = s { ast     = as  }

instance Show SizeState where
    show = showSappState

----------------------------------------
-- Initial

initialState :: SizeState
initialState = SizeState
    { table    = emptyTable
    , stack    = topStack
    , scopeId  = topScopeNum
    , ast      = Program empty
    , offStack = singletonStack 0
    }

--------------------------------------------------------------------------------
-- Building the Monad

buildSizeOffset :: SappWriter -> SymbolTable -> Program -> SizeOffset ()
buildSizeOffset w tab program@(Program block) = do
    modify $ \s -> s { table = tab, ast = program }
    tell w
    sizeOffsetStatements block

----------------------------------------

--------------------------------------------------------------------------------
-- Using the Monad

processSizeOffset :: SappWriter -> SymbolTable -> Program -> (SizeState, SappWriter)
processSizeOffset w tab = runProgramSizeOffset . buildSizeOffset w tab

runProgramSizeOffset :: SizeOffset a -> (SizeState, SappWriter)
runProgramSizeOffset = (\(_,s,w) -> (s,w)) . runSizeOffset

runSizeWithReader :: SappReader -> SizeOffset a -> (a, SizeState, SappWriter)
runSizeWithReader r = flip (flip runRWS r) initialState

runSizeOffset :: SizeOffset a -> (a, SizeState, SappWriter)
runSizeOffset = runSizeWithReader initialReader

getSizeOffset :: SizeOffset a -> a
getSizeOffset = (\(v,_,_) -> v) . runSizeOffset

getWriter :: SizeOffset a -> SappWriter
getWriter = (\(_,_,w) -> w) . runSizeOffset

getState :: SizeOffset a -> SizeState
getState = (\(_,s,_) -> s) . runSizeOffset

--------------------------------------------------------------------------------
-- Monad handling

enterFunction :: SizeOffset ()
enterFunction = modify $ \s -> s { offStack = push 0 (offStack s) }

exitFunction :: SizeOffset Offset
exitFunction = do
    off <- gets (top . offStack)
    modify $ \s -> s { offStack = pop $ offStack s }
    return off

currentOffset :: SizeOffset Offset
currentOffset = gets (top . offStack)

addOffset :: Offset -> SizeOffset ()
addOffset off = modify $ \s -> s { offStack = modifyStack (+off) (offStack s) }

resetOffset :: SizeOffset ()
resetOffset = modify $ \s -> s { offStack = push 0 $ pop (offStack s) }


--------------------------------------------------------------------------------
-- Statements

sizeOffsetStatements :: StBlock -> SizeOffset ()
sizeOffsetStatements = mapM_ sizeOffsetStatement


sizeOffsetStatement :: Lexeme Statement -> SizeOffset ()
sizeOffsetStatement stL = case lexInfo stL of

    StVariableDeclaration dclL -> do
        let idn = lexInfo . dclIdentifier $ lexInfo dclL
        (symStk, symWdt) <- liftM fromJust $ getsSymbol idn (scopeStack &&& width)
        off              <- currentOffset
        modifySymbolWithScope idn symStk (\sym -> sym { offset = off })
        addOffset symWdt

    StStructDefinition _ -> enterScope >> exitScope         -- For scopeStack maintenance

    StFunctionDef (Lex idn _) (Sign parms _) block ->  do
        sym <- liftM fromJust $ getSymbol idn

        enterFunction
        enterScope

        -- Parameters offsets
        forM_ parms $ \(Lex dcl _) -> do
            let parmIdn = lexInfo $ dclIdentifier dcl
            (parmStk, parmWdt) <- liftM fromJust $ getsSymbol parmIdn (scopeStack &&& width)
            off                <- currentOffset
            modifySymbolWithScope parmIdn parmStk (\sym' -> sym' { offset = (-1) * off }) -- (-1) For the execution stack
            addOffset parmWdt

        -- Restarts the offset in 0 for this scope
        parmsWdt <- currentOffset
        resetOffset

        sizeOffsetStatements block
        exitScope

        blockWdt <- exitFunction
        modifySymbolWithScope idn (scopeStack sym) (\sym' -> sym' { width = blockWdt + parmsWdt })

    StIf _ trueBlock falseBlock -> do
        enterScope
        sizeOffsetStatements trueBlock
        exitScope

        enterScope
        sizeOffsetStatements falseBlock
        exitScope

    StCase _ whnLs othrBlock -> do
        forM_ whnLs $ \(Lex (When _ wBlock) _) -> do
            enterScope
            sizeOffsetStatements wBlock
            exitScope

        enterScope
        sizeOffsetStatements othrBlock
        exitScope

    StLoop befBlock _ aftBlock -> do
        enterScope
        sizeOffsetStatements befBlock
        exitScope

        enterScope
        sizeOffsetStatements aftBlock
        exitScope

    StFor (Lex idn _) _ block -> do
        enterScope
        (symStk, symWdt) <- liftM fromJust $ getsSymbol idn (scopeStack &&& width)
        off              <- currentOffset
        modifySymbolWithScope idn symStk (\sym -> sym { offset = off })
        addOffset symWdt

        sizeOffsetStatements block
        exitScope

    _ -> return ()
    --StAssign (Lexeme Access) (Lexeme Expression)
    --StStructDefinition    (Lexeme DataType)
    --StReturn        (Lexeme Expression)
    --StProcedureCall (Lexeme Identifier) (Seq (Lexeme Expression))
    --StRead       (Lexeme Access)
----    --StPrint      (Lexeme Expression)
    --StBreak
    --StContinue

--------------------------------------------------------------------------------
-- Symbol Table processing

-- processOffset :: Seq (Identifier, Symbol) -> SizeOffset ()
-- processOffset syms = forM_ syms $ \(idn, sym) ->
--     case symbolCategory sym of
--         CatInfo     -> undefined
--         CatType     -> undefined
--         CatFunction -> undefined
