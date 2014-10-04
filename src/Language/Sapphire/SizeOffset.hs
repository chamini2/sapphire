module Language.Sapphire.SizeOffset
    ( SizeState

    , SizeOffset
    , processSizeOffset
    ) where

import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad
import           Language.Sapphire.SymbolTable

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (liftM)
import           Control.Monad.RWS             (RWS, runRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (forM_, mapM_)
import           Data.Maybe                    (fromJust)
import           Data.Sequence                 (empty)
import           Prelude                       hiding (mapM_)

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

processSizeOffset :: SappReader -> SappWriter -> SymbolTable -> Program -> (SizeState, SappWriter)
processSizeOffset r w tab = runSizeOffset r . buildSizeOffset w tab

runSizeOffset :: SappReader -> SizeOffset a -> (SizeState, SappWriter)
runSizeOffset r = (\(_,s,w) -> (s,w)) . flip (flip runRWS r) initialState

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

    StStructDefinition _ _ -> enterScope >> exitScope       -- For scopeStack maintenance

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
    --StAssign
    --StStructDefinition
    --StReturn
    --StProcedureCall
    --StRead
    --StPrint
    --StBreak
    --StContinue
