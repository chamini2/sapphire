{-|
    MIPS code generation module
 -}

module Language.Sapphire.MIPSGenerator 
    ( MIPSGenerator
    , processMIPSGenerator
    ) where

import           Language.Sapphire.MIPS        as MIPS             
import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad   hiding (initialWriter)
import           Language.Sapphire.SymbolTable
import           Language.Sapphire.TAC as TAC  hiding (generate(..), Label)

import           Control.Monad.RWS             (RWS, execRWS, lift)
import           Control.Monad.State           (get, gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (mapM_)
import           Data.Sequence                 (Seq, empty, singleton)
import           Prelude                       hiding (mapM_)

--------------------------------------------------------------------------------

type MIPSGenerator = RWS MIPSReader MIPSWriter MIPSState

--------------------------------------------------------------------------------
-- Reader

type MIPSReader = ()

--------------------------------------------------------------------------------
-- State

data MIPSState = MIPSState
    { table       :: SymbolTable
    , stack       :: Stack Scope
    , scopeId     :: Scope
    , ast         :: Program
    }

----------------------------------------
-- Instances

instance SappState MIPSState where
    getTable   = table
    getStack   = stack
    getScopeId = scopeId
    getAst     = ast
    putTable   tab s = s { table   = tab }
    putStack   stk s = s { stack   = stk }
    putScopeId sc  s = s { scopeId = sc  }
    putAst     as  s = s { ast     = as  }

instance Show MIPSState where
    show = showSappState

----------------------------------------
-- Initial

initialState :: MIPSState
initialState = MIPSState
    { table       = emptyTable
    , stack       = globalStack
    , scopeId     = globalScope
    , ast         = Program empty
    }

--------------------------------------------------------------------------------
-- Writer

type MIPSWriter = Seq MIPS.Instruction

----------------------------------------
-- Initial

initialWriter :: MIPSWriter
initialWriter = empty

----------------------------------------

generate :: MIPS.Instruction -> MIPSGenerator ()
generate = tell . singleton

--------------------------------------------------------------------------------
-- Building the Monad

buildMIPSGenerator :: SymbolTable -> TAC -> MIPSGenerator ()
buildMIPSGenerator tab tac = do
    modify $ \s -> s { table = tab }
    tell initialWriter
    mapM_ tac2Mips tac

--------------------------------------------------------------------------------
-- Using the Monad

processMIPSGenerator :: MIPSReader -> SymbolTable -> TAC -> MIPSWriter
processMIPSGenerator r s = generateMIPS r . buildMIPSGenerator s

generateMIPS :: MIPSReader -> MIPSGenerator a -> MIPSWriter
generateMIPS r = snd . flip (flip execRWS r) initialState

----------------------------------------

tac2Mips :: TAC.Instruction -> MIPSGenerator ()
tac2Mips tac = case tac of 
      {-Comment str          -> generate $ MIPS.Comment str-}
      {-PutLabel l b str     -> undefined-}
      AssignBin res op l r -> undefined
      AssignUn res unop op -> undefined
      Assign dst src       -> undefined
--    | AssignArrR
--    | AssignArrL
    -- Function related instructions
      BeginFunction w   -> undefined
      EndFunction       -> undefined
      PushParameter ref -> undefined
--    | PopParameters Int
      Return mayA     -> undefined
      PCall lab nInt  -> undefined
      FCall ref lab n -> undefined
    -- Print
      PrintInt    ref     -> undefined
      PrintFloat  ref     -> undefined
      PrintChar   ref     -> undefined
      PrintBool   ref     -> undefined
      PrintString off wdt -> undefined
    -- Read
      ReadInt   ref -> undefined
      ReadFloat ref -> undefined
      ReadChar  ref -> undefined
      ReadBool  ref -> undefined
    -- Goto
      Goto lab                  -> undefined
      IfGoto      rel le ri lab -> undefined
      IfTrueGoto  ref lab       -> undefined
      IfFalseGoto ref lab       -> undefined
