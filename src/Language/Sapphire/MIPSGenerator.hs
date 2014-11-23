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

import           Control.Monad                 (liftM, unless)
import           Control.Monad.RWS             (RWS, execRWS, lift)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (get, gets, modify)
import           Control.Monad.Writer          (tell)
import           Data.Foldable                 (mapM_, forM_)
import           Data.Sequence                 (Seq, empty, singleton)
import           Prelude                       hiding (mapM_, EQ, LT, GT)

--------------------------------------------------------------------------------

type MIPSGenerator = RWS SappReader MIPSWriter MIPSState

--------------------------------------------------------------------------------
-- State

data MIPSState = MIPSState
    { table       :: SymbolTable
    , stack       :: Stack Scope
    , scopeId     :: Scope
    , ast         :: Program

    , registerDescriptors
    , variablesDescriptors
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

processMIPSGenerator :: SappReader -> SymbolTable -> TAC -> MIPSWriter
processMIPSGenerator r s = generateMIPS r . buildMIPSGenerator s

generateMIPS :: SappReader -> MIPSGenerator a -> MIPSWriter
generateMIPS r = snd . flip (flip execRWS r) initialState

emitPreamble :: MIPSGenerator ()
emitPreamble = do
    filePath <- asks file
    generate $ MIPS.Comment $ filePath ++ " - Sapphire compiler generated MIPS code"

    --  Generate .data section
    generateGlobals

    generate $ MIPS.PutLabel ".text"       ""
    generate $ MIPS.PutLabel ".align 2"    ""
    generate $ MIPS.PutLabel ".globl main" ""

{-
 -    Generates MIPS code for printing scalar data types
 -
 -    List of codes used by syscall
 -    1  - Print Integer      $A0  Integer to print
 -    2  - Print Float        $F12 Float to print
 -    3  - Print Double       $F12 Double to print
 -    4  - Print String       $A0  Address of null terminated string to print
 -    11 - Print Character    
 -    5  - Read Integer
 -    6  - Read Float
 -    7  - Read Double
 -    8  - Read String 
 -    12 - Read Character
 -
 -}
generatePrint :: MIPS.Label -> Int -> MIPSGenerator ()
generatePrint lab code = do
    generate $ Li V0 (Const code)
    generate $ La A0 (Label lab)
    generate Syscall

generatePrintString :: Offset -> MIPSGenerator ()
generatePrintString off = generatePrint ("_string" ++ show off) 4

generateRead :: Label -> Int -> MIPSGenerator ()
generateRead fname code = return ()

generateGlobals :: MIPSGenerator ()
generateGlobals = do
    generate $ MIPS.PutLabel ".data" ""
    syms <- liftM toSeq $ gets getTable
    forM_ syms $ \(idn, sym) -> do
        case sym of
            SymInfo { dataType = Lex String _ , offset = off } -> generate $ Asciiz ("_string" ++ show off) idn
            otherwise                -> return ()
     {-mapM_ (\(c, n) generateString) -}

----------------------------------------
--  Register allocation

{-|
 -  Given a Reference, say r, of a current var or temporary, 
 -  a reason (to read or to write), this function will assign 
 -  a register for r according to the following rules:
 -
 -  - A register already holding r 
 -  - An empty register
 -  - An unmodified register
 -  - A modified (dirty) register. We have to spill it
 -
 -}
{-getRegister :: Reference -> Reason -> MIPSGenerator Register-}
getRegister :: Reason -> Reference -> MIPSGenerator Register
getRegister reason ref = findRegisterWithContents ref

data Reason = Read | Write

getRegisterForWrite :: Reference -> MIPSGenerator Register
getRegisterForWrite = getRegister Write

findRegisterWithContents :: Reference -> MIPSGenerator Register
findRegisterWithContents ref = return Zero

spillRegister :: Register -> MIPSGenerator ()
spillRegister reg = return ()

spillAllDirtyRegisters :: MIPSGenerator ()
spillAllDirtyRegisters = return ()

buildOperand :: Reference -> MIPSGenerator Operand
buildOperand ref = case ref of
    Address   iden ref glob -> do
        return $ Indexed 99 T1
    Constant  val -> do
        case val of 
            ValInt int -> return $ Const int
            otherwise  -> return $ Const 101
    Temporary int -> return $ Const 101

{-generateLoadConstant :: Reference -> Int -> MIPSGenerator ()-}
{-generateLoadConstant dst imm = do-}
    {-reg <- getRegisterForWrite dst-}
    {-generate $ Li reg (Const imm)-}

{-generateLoadAddress :: Reference -> MIPS.Label -> MIPSGenerator ()-}
{-generateLoadAddress dst lab = do-}
    {-reg <- getRegisterForWrite dst-}
    {-generate $ La reg lab-}

----------------------------------------

tac2Mips :: TAC.Instruction -> MIPSGenerator ()
tac2Mips tac = case tac of 
      TAC.Comment str      -> generate $ MIPS.Comment str

      TAC.PutLabel lab str -> do
        {-spillAllDirtyRegisters  -}
        if (lab == "_main")
            then generate $ MIPS.PutLabel "main:" str 
            else generate $ MIPS.PutLabel (lab ++ ":") str

      AssignBin res op l r -> do
        rDst   <- getRegister res

        lef    <- getRegister l
        opl    <- buildOperand l
        generate $ Ld lef opl

        rig    <- getRegister r
        opr    <- buildOperand l
        generate $ Ld rig opr

        case op of 
            ADD   -> generate $ Add rDst lef rig
            SUB   -> generate $ Sub rDst lef rig
            MUL   -> generate $ Mul rDst lef rig
            DIV   -> do
                generate $ Div lef rig
                generate $ Mflo rDst
            MOD   -> do
                generate $ Div lef rig
                generate $ Mfhi rDst
            {-POW   -> "^"-}
            OR    -> generate $ Or  rDst lef rig
            AND   -> generate $ And rDst lef rig

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
        generate $ Subu  SP SP (Const 8)            -- Decrement $sp to make space to save $ra, $fp
        {-generate $ Subu  SP SP (Const 12)           -- Decrement $sp to make space to save $ra, $fp and return value-}
        generate $ Sw    FP (Indexed 8 SP)          -- Save fp
        generate $ Sw    RA (Indexed 4 SP)          -- Save ra
                                                    -- Save return value
        generate $ Addiu FP SP (Const 8)            -- Setup new fp
        if (w == 0) 
            then generate $ Subu SP SP (Const w)    -- Decrement sp to make space for locals/temps
            else return ()     

      EndFunction       -> do
        return ()
        -- We could have an implicit return value here, but in our case return statements are mandatory

      PushParameter ref -> do
        generate $ Subu SP SP (Const 4)     -- Decrement sp to make space for param
        reg <- getRegister ref
        generate $ Sw reg (Indexed 4 SP)    -- Copy param value to stack

      PopParameters int -> do
        generate $ Addi SP SP (Const int)   -- Pop params of stack

      Return mayA     -> do 
        case mayA of
            Just ref -> do
                {-Register reg <- getRegister ref-}
                generate $ Move V0 T5
                return ()
            Nothing  -> return ()
        generate $ Move SP FP               -- Pop callee frame off stack
        {-generate $ Lw FP (Indexed (-8)  FP)    -- Restore saved return value-}
        generate $ Lw RA (Indexed (-4) FP)  -- Restore saved ra
        generate $ Lw FP (Indexed 0  FP)    -- Restore saved fp
        generate $ Jr RA                    -- Return from function

      PCall lab nInt  -> do
        generate $ Jal ("_" ++ lab) 

      FCall ref lab n -> do
        generate $ Jal ("_" ++ lab) 
        -- get return value TO DO
        ret <- getRegisterForWrite ref
        generate $ Move ret V0 -- Copy return value from $v0 - MIPS Convention
        
      -- Print
      PrintInt    ref   -> return () --generatePrintInt ref
      PrintFloat  ref   -> return ()
      PrintChar   ref   -> return ()
      PrintBool   ref   -> return ()
      PrintString off _ -> generatePrintString off

      -- Read
      ReadInt   ref -> return ()
      ReadFloat ref -> return ()
      ReadChar  ref -> return ()
      ReadBool  ref -> return ()
      -- Goto
      Goto lab                  -> do
        spillAllDirtyRegisters 
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
