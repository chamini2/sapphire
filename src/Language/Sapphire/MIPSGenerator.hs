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
import           Data.Map.Strict               as Map (Map, empty, insert,
                                                singleton, toList, member,
                                                adjust)
import           Prelude                       hiding (mapM_, EQ, LT, GT)

--------------------------------------------------------------------------------

type MIPSGenerator = RWS SappReader MIPSWriter MIPSState

--------------------------------------------------------------------------------
-- State

data MIPSState = MIPSState
    { table               :: SymbolTable
    , stack               :: Stack Scope
    , scopeId             :: Scope
    , ast                 :: Program

    , registerDescriptors :: Map.Map Register RegDescriptor

    {-, variablesDescriptors :: -}
    }

initialRegDescriptors :: RegDescriptorTable
initialRegDescriptors = Map.fromList [
        (T0, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T1, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T2, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T3, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T4, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T5, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T6, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T7, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T8, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (T9, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S0, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S1, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S2, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S3, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S4, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S5, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S6, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
        (S7, RegDescriptor { value = Nothing, isGeneral = Tru, isDirty = False })
    ]                                                      

data RegDescriptor = RegDescriptor
    { value     :: Maybe Reference
    , isGeneral :: Bool
    , isDirty   :: Bool
    } 

{-data VarDescriptor = VarDescriptor -}
    {-{ var       :: Reference-}
    {-, locations :: Reference-}
    {-}-}

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

    , registerDescriptors = initialRegDescriptors
    {-, variablesDescriptors-}
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
generatePrintString :: Offset -> MIPSGenerator ()
generatePrintString off  = do
    generate $ Li V0 (Const 4)
    generate $ La A0 (Label ("_string" ++ show off))
    generate Syscall

generatePrint :: Reference -> MIPS.Code -> MIPSGenerator ()
generatePrint ref code  = do
    generate $ Li V0 (Const code)
    {-generate $ Lw A0 (Indexed int reg) -}
    generate Syscall

generateRead :: Reference -> MIPS.Code -> MIPSGenerator ()
generateRead ref code = do
    generate $ Li V0 (Const code)
    {-generate $ Lw A0 (Indexed int reg) -}
    generate Syscall

generateGlobals :: MIPSGenerator ()
generateGlobals = do
    generate $ MIPS.PutLabel ".data" ""
    syms <- liftM toSeq $ gets getTable
    forM_ syms $ \(idn, sym) -> do
        case sym of
            SymInfo { dataType = Lex String _ , offset = off } -> generate $ Asciiz ("_string" ++ show off) idn
            otherwise                                          -> return ()
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

data Reason = Read | Write

getRegister :: Reason -> Reference -> MIPSGenerator Register
getRegister reason ref = do
    mReg <- findRegisterWithContents (Just ref)
    case mReg of
        Just reg -> return reg 
        Nothing -> do
            mReg <- findRegisterWithContents Nothing
            case mReg of
                Just reg -> return reg
                Nothing  -> return ()
    if reason == Write then markDirty reg else return ()

getRegisterForWrite :: Reference -> MIPSGenerator Register
getRegisterForWrite = getRegister Write

findRegisterWithContents :: Maybe Reference -> MIPSGenerator (Maybe Register)
findRegisterWithContents ref = do
    regs <- gets registerDescriptor
    let reg = fold checkRegister Nothing regs 
    where checkRegister r mReg = case mReg of
            Just reg -> Just mReg
            Nothing  -> if locationsAreTheSame ref then else 

locationsAreSame :: Reference -> Reference -> MIPSGenerator ()
locationsAreSame r1 r2 = return ()

markDirty :: Register -> MIPSGenerator ()
markDirty reg = do
    regDes <- gets registerDescriptors 
    if Map.member reg regDes 
        then Map.adjust (\rd -> rd { isDirty = True }) reg regDes
        else error "MIPSGenerator.markDirty: marking unexistant register as dirty"

spillRegister :: Register -> MIPSGenerator ()
spillRegister reg = return ()

spillAllDirtyRegisters :: MIPSGenerator ()
spillAllDirtyRegisters = return ()

buildOperand :: Reference -> MIPSGenerator Operand
buildOperand ref = case ref of
    Address   iden ref glob -> do
        return $ Indexed 1 T1
    Constant  val -> do
        case val of 
            ValInt int -> return $ Const int
            otherwise  -> return $ Const 2
    Temporary int -> return $ Const 3

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

      AssignBin x op y z -> do
        ry  <- getRegister y
        opl <- buildOperand y
        generate $ Ld ry opl

        rz  <- getRegister z
        opr <- buildOperand y
        generate $ Ld rz opr

        rx  <- getRegisterForWrite x

        case op of 
            ADD -> generate $ Add rx ry rz
            SUB -> generate $ Sub rx ry rz
            MUL -> generate $ Mul rx ry rz
            DIV -> generate (Div ry rz) >> generate (Mflo rx)
            MOD -> generate (Div ry rz) >> generate (Mfhi rx)
            {-POW   -> "^"-}
            OR  -> generate $ Or  rx ry rz
            AND -> generate $ And rx ry rz

      AssignUn res unop op -> do
        case unop of 
            NOT -> return () -- Boolean not
            NEG -> return () -- Arithmetic negation

      Assign x y -> do
        rx <- getRegisterForWrite x
        op <- buildOperand y
        case op of
            Register ry         -> generate $ Move rx ry
            immm@(Const imm)    -> generate $ Li rx immm
            ind@(Indexed int r) -> generate $ Lw rx ind

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
      PrintInt    ref   -> generatePrint ref 1
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
