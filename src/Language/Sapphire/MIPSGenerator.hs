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
import           Prelude                       hiding (mapM_, EQ, LT, GT)

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
    emitPreamble
    mapM_ tac2Mips tac

--------------------------------------------------------------------------------
-- Using the Monad

processMIPSGenerator :: MIPSReader -> SymbolTable -> TAC -> MIPSWriter
processMIPSGenerator r s = generateMIPS r . buildMIPSGenerator s

generateMIPS :: MIPSReader -> MIPSGenerator a -> MIPSWriter
generateMIPS r = snd . flip (flip execRWS r) initialState

emitPreamble :: MIPSGenerator ()
emitPreamble = do
    {-generate $ PutLabel fileName ""-}
    generate $ MIPS.PutLabel ".text"       ""
    generate $ MIPS.PutLabel ".align 2"    ""
    generate $ MIPS.PutLabel ".globl main" ""

    generatePrint "_PrintInt"    1
    generatePrint "_PrintString" 4
    {-generatePrint "_PrintChar"   11-}

generatePrint :: Label -> Int -> MIPSGenerator ()
generatePrint fname code = do
    generate $ MIPS.PutLabel fname ("Language defined " ++ fname)
    generate $ Li V0 (Const code)
    generate $ Lw A0 (Indexed 0 FP)
    generate Syscall

getRegister :: Reference -> MIPSGenerator Register
getRegister ref = return T0

spillRegister :: Register -> MIPSGenerator ()
spillRegister reg = undefined

buildOperand :: Reference -> MIPSGenerator Operand
buildOperand ref = case ref of
    Address   iden ref glob -> do
        return $ Indexed 99 T1
    Constant  val           -> do
        case val of 
            ValInt int -> return $ Const int
            otherwise  -> return $ Const 101
    Temporary int           -> return $ Const 101

----------------------------------------

tac2Mips :: TAC.Instruction -> MIPSGenerator ()
tac2Mips tac = case tac of 
      TAC.Comment str      -> generate $ MIPS.Comment str

      TAC.PutLabel lab str -> do
        case head lab of 
            'L'       -> return ()
            otherwise -> generate $ MIPS.PutLabel lab str

      AssignBin res op l r -> do
        regRes <- getRegister res
        lef    <- getRegister l
        opl    <- buildOperand l
        generate $ Ld lef opl
        rig    <- getRegister r
        opr    <- buildOperand l
        generate $ Ld rig opr
        case op of 
            ADD   -> generate $ Add regRes lef rig
            SUB   -> generate $ Sub regRes lef rig
            MUL   -> generate $ Mul regRes lef rig
            DIV   -> do
                generate $ Div lef rig
                generate $ Mflo regRes
            MOD   -> do
                generate $ Div lef rig
                generate $ Mfhi regRes
            {-POW   -> "^"-}
            OR    -> generate $ Or  regRes lef rig
            AND   -> generate $ And regRes lef rig

      AssignUn res unop op -> do
        case unop of 
            NOT -> return () -- tac2Mips $ AssignBin Sub op  -- Boolean not
            NEG -> return () -- tac2Mips $     -- Arithmetic negation

      Assign dst src       -> do
        reg <- getRegister  dst
        op  <- buildOperand src
        case op of
            Register r          -> generate $ Move reg r
            immm@(Const imm)    -> generate $ Li reg immm
            ind@(Indexed int r) -> generate $ Lw reg ind

--    | AssignArrR
--    | AssignArrL
    -- Function related instructions
      BeginFunction w   -> do
        generate $ Subu  SP SP (Const 8)    -- Decrement $sp to make space to save $ra, $fp
        generate $ Sw    FP (Indexed 8 SP)  -- Save fp
        generate $ Sw    RA (Indexed 4 SP)  -- Save ra
        generate $ Addiu FP SP (Const 8)    -- Setup new fp
        generate $ Subu SP SP (Const w)     -- Decrement sp to make space for locals/temps

      EndFunction       -> return ()

      PushParameter ref -> do
        generate $ Subu SP SP (Const 4)     -- Decrement sp to make space for param
        reg <- getRegister ref
        generate $ Sw reg (Indexed 4 SP)    -- Copy param value to stack

      PopParameters int -> do
        generate $ Addi SP SP (Const int)   -- Pop params of stack

      Return mayA     -> do 
        case mayA of
            Just ref -> do
                {-Register reg <- buildOperand ref-}
                {-generate $ Move V0 reg-}
                return ()
            Nothing  -> return ()
        generate $ Move SP FP               -- Pop calee frame off stack
        generate $ Lw RA (Indexed (-4) FP)  -- Restore saved ra
        generate $ Lw FP (Indexed 0  FP)    -- Restore saved fp
        generate $ Jr RA                    -- Return from function

      PCall lab nInt  -> do
        generate $ Jal ("FUN_" ++ lab) 

      FCall ref lab n -> do
        generate $ Jal ("FUN_" ++ lab) 
        -- get return value TO DO
        
      -- Print
      PrintInt    ref     -> do
        {-tac2Mips $ PushParameter ref-}
        tac2Mips $ PCall "_PrintInt" 1

      PrintFloat  ref     -> return ()
      PrintChar   ref     -> return ()
      PrintBool   ref     -> return ()
      PrintString off wdt -> return ()
      -- Read
      ReadInt   ref -> return ()
      ReadFloat ref -> return ()
      ReadChar  ref -> return ()
      ReadBool  ref -> return ()
      -- Goto
      Goto lab                  -> do
        generate $ B lab        --  Unconditional branch
      IfGoto      rel le ri lab -> do
        regLe <- getRegister le 
        ople  <- buildOperand le
        generate $ Ld regLe ople
        regRi <- getRegister ri
        opri  <- buildOperand ri
        generate $ Ld regRi opri
        case rel of
            EQ -> do
                generate $ Sub regLe regLe regRi
                generate $ Beqz regLe lab
            NE -> do
                generate $ Sub regLe regLe regRi
                generate $ Bnez regLe lab
            LT -> do
                generate $ Sub regLe regLe regRi
                generate $ Bgtz regLe lab
            LE -> do
                generate $ Sub regLe regLe regRi
                generate $ Bgez regLe lab
            GT -> do
                generate $ Sub regLe regLe regRi
                generate $ Bltz regLe lab
            GE -> do
                generate $ Sub regLe regLe regRi
                generate $ Blez regLe lab
        
      IfTrueGoto  ref lab       -> do
        reg <- getRegister ref
        generate $ Beqz reg lab
      IfFalseGoto ref lab       -> do
        reg <- getRegister ref
        generate $ Bnez reg lab
      _                         -> error "MIPSGenerator.tac2Mips unknown instruction"
