module Language.Sapphire.SizeOffset
    ( SizeState

    , SizeOffset
    , processSizeOffset
    ) where

import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad
import           Language.Sapphire.SymbolTable

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (liftM, unless)
import           Control.Monad.RWS             (RWS, execRWS)
import           Control.Monad.State           (gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (forM_, mapM_)
import           Data.Maybe                    (fromJust, isJust)
import           Data.Sequence                 (empty)
import           Prelude                       hiding (mapM_, exp)

--------------------------------------------------------------------------------

type SizeOffset = RWS SappReader SappWriter SizeState

--------------------------------------------------------------------------------
-- State

data SizeState = SizeState
    { table        :: SymbolTable
    , stack        :: Stack Scope
    , scopeId      :: Scope
    , ast          :: Program
    , offStack     :: Stack Offset
    , globalOffset :: Offset
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
    { table        = emptyTable
    , stack        = globalStack
    , scopeId      = globalScope
    , ast          = Program empty
    , offStack     = emptyStack
    , globalOffset = 0
    }

--------------------------------------------------------------------------------
-- Building the Monad

buildSizeOffset :: SappWriter -> SymbolTable -> Program -> SizeOffset ()
buildSizeOffset w tab program@(Program block) = do
    modify $ \s -> s { table = tab, ast = program }
    tell w
    sizeOffsetStatements block

--------------------------------------------------------------------------------
-- Using the Monad

processSizeOffset :: SappReader -> SappWriter -> SymbolTable -> Program -> (SizeState, SappWriter)
processSizeOffset r w tab = runSizeOffset r . buildSizeOffset w tab

runSizeOffset :: SappReader -> SizeOffset a -> (SizeState, SappWriter)
runSizeOffset r = flip (flip execRWS r) initialState

--------------------------------------------------------------------------------
-- Monad handling

enterFunction :: SizeOffset ()
enterFunction = modify $ \s -> s { offStack = push 0 (offStack s) }

exitFunction :: SizeOffset Offset
exitFunction = do
    offStk <- gets offStack
    modify $ \s -> s { offStack = pop offStk }
    return $ top offStk

currentOffset :: SizeOffset Offset
currentOffset = do
    scp <- currentScope
    if scp == globalScope
        then gets globalOffset
        else gets (top . offStack)

addOffset :: Offset -> SizeOffset ()
addOffset off = do
    scp <- currentScope
    if scp == globalScope
        then modify $ \s -> s { globalOffset = globalOffset s - off }
        else modify $ \s -> s { offStack = touch (\t -> t - off) (offStack s) }

resetOffset :: SizeOffset ()
resetOffset = exitFunction >> enterFunction

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
        modifySymbolWithScope idn symStk $ \sym -> sym { offset = off }
        addOffset symWdt

    StFunctionDef (Lex idn _) (Sign prms _) block ->  do
        sym <- liftM fromJust $ getSymbol idn

        enterFunction
        enterScope

        addOffset 4 -- For MIPS parameters

        -- Parameters offsets
        forM_ prms $ \(Lex dcl _) -> do
            let prmIdn = lexInfo $ dclIdentifier dcl
            (prmstk, prmWdt) <- liftM fromJust $ getsSymbol prmIdn (scopeStack &&& width)
            off              <- currentOffset
            modifySymbolWithScope prmIdn prmstk $ \sym' -> sym' { offset = negate off }     -- (-1) For the execution stack
            addOffset prmWdt

        -- Restarts the offset in 0 for the statements block
        prmsWdt <- currentOffset
        resetOffset

        addOffset 8 -- For MIPS parameters

        sizeOffsetStatements block
        exitScope

        blockWdt <- exitFunction
        modifySymbolWithScope idn (scopeStack sym) $ \sym' -> sym' { blockWidth = negate blockWdt, prmsWidth = negate prmsWdt }

    StPrint expL -> addStrings expL

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
        modifySymbolWithScope idn symStk $ \sym -> sym { offset = off }
        addOffset symWdt

        sizeOffsetStatements block
        exitScope

    _ -> return ()
    -- StAssign
    -- StStructDefinition
    -- StReturn
    -- StProcedureCall
    -- StRead
    -- StBreak
    -- StContinue

--------------------------------------------------------------------------------

-- We only need to check for Strings to add them to the SymbolTable
addStrings :: Lexeme Expression -> SizeOffset ()
addStrings (Lex exp _) = case exp of

    LitString strL -> do
        strOff <- gets globalOffset
        let strWdt = length $ lexInfo strL
            info = emptySymInfo
                { dataType   = pure String
                , category   = CatConstant
                , offset     = strOff
                , width      = strWdt
                , used       = True
                , scopeStack = globalStack
                , defPosn    = lexPosn strL
                }
        let idn = (show $ lexInfo strL)
        maySymbol <- getSymbol idn
        unless (isJust maySymbol) $ do
            -- Set the new global offset
            modify $ \s -> s { globalOffset = strOff + strWdt}
            addSymbol idn info

    _ -> return ()
