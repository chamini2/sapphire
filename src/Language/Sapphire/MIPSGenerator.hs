{-|
    MIPS code generation module
 -}

module Language.Sapphire.MIPSGenerator 
    ( MIPSGenerator
    , processMIPSGenerator
    ) where

import           Language.Sapphire.MIPS                  
import           Language.Sapphire.Program
import           Language.Sapphire.SappMonad   hiding (initialWriter)
import           Language.Sapphire.SymbolTable
import           Language.Sapphire.TAC         hiding (generate, Instruction(..), Label)

import           Control.Monad.RWS             (RWS, execRWS, lift)
import           Control.Monad.State           (get, gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Sequence                 (Seq, empty, singleton)

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

type MIPSWriter = Seq Instruction

----------------------------------------
-- Initial

initialWriter :: MIPSWriter
initialWriter = empty

----------------------------------------

generate :: Instruction -> MIPSGenerator ()
generate = tell . singleton

--------------------------------------------------------------------------------
-- Building the Monad

buildMIPSGenerator :: SymbolTable -> TAC -> MIPSGenerator ()
buildMIPSGenerator tab tac = do
    modify $ \s -> s { table = tab }
    tell initialWriter
    tac2Mips tac

--------------------------------------------------------------------------------
-- Using the Monad

processMIPSGenerator :: MIPSReader -> SymbolTable -> TAC -> MIPSWriter
processMIPSGenerator r s = generateMIPS r . buildMIPSGenerator s

generateMIPS :: MIPSReader -> MIPSGenerator a -> MIPSWriter
generateMIPS r = snd . flip (flip execRWS r) initialState

----------------------------------------

tac2Mips :: TAC -> MIPSGenerator ()
tac2Mips tac = generate $ Comment "PROBANDO"    --case ins of
      {-Comment String = -}
    {-| PutLabel Label StGring-}
    {-| AssignBin-}
        {-{ result :: Reference-}
        {-, binop  :: BinOperator-}
        {-, left   :: Reference-}
        {-, right  :: Reference-}
        {-}-}
    {-| AssignUn-}
        {-{ result  :: Reference-}
        {-, unop    :: UnOperator-}
        {-, operand :: Reference-}
        {-}-}
    {-| Assign-}
        {-{ dst :: Reference-}
        {-, src :: Reference-}
        {-}-}
{---    | AssignArrR-}
{---    | AssignArrL-}
    {--- Function related instructions-}
    {-| BeginFunction Width-}
    {-| EndFunction-}
    {-| PushParameter Reference-}
{---    | PopParameters Int-}
    {-| Return (Maybe Reference)-}
    {-| PCall           Label Int-}
    {-| FCall Reference Label Int-}
    {--- Print-}
    {-| PrintInt    Reference-}
    {-| PrintFloat  Reference-}
    {-| PrintChar   Reference-}
    {-| PrintBool   Reference-}
    {-| PrintString Offset    Width-}
    {--- Read-}
    {-| ReadInt   Reference-}
    {-| ReadFloat Reference-}
    {-| ReadChar  Reference-}
    {-| ReadBool  Reference-}
    {--- Goto-}
    {-| Goto Label-}
    {-| IfGoto      Relation Reference Reference Label-}
    {-| IfTrueGoto  Reference                    Label-}
    {-| IfFalseGoto Reference                    Label-}
