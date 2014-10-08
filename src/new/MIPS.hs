{-|
    MIPS code generation module
-}
module MIPS (
      MIPSGenerator
    , generateMIPS
) 
where

import           Data.Sequence             as DS (Seq, empty, length)
import           Data.Foldable             as DF (mapM_, all, and, forM_)
import           Data.Traversable          (mapM)
import           Prelude                   as P hiding (mapM_, mapM, all, and)


{-|
    MIPS relative information and data types
-}

type Label = String

{-|
    MIPS 32 registers
 -}
data Register = 
    | Zero                                  -- Constant register
--  | AT                                    -- Reserved for assembler
    | A0 | A1 | A2 | A3                     -- Used to pass first four arguments to function call
    | V0 | V1 |                             -- Used to return results of functions
    | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 -- General purpose (caller saved)
    | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 -- General purpose (callee saved)
    | T8 | T9                               -- General purpose (caller saved)
--  | K0 | K1                               -- Reserved for OS
    | GP                                    -- Global pointer
    | FP                                    -- Frame pointer
    | SP                                    -- Stack pointer
    | RA                                    -- Record address

{-|
    MIPS 32 floating-point registers
-}
data FlPRegister = 
    | Zero                                  -- Constant register
    | AT                                    -- Reserved for assembler
    | A0 | A1 | A2 | A3                     -- Used to pass first four arguments to function call
    | V0 | V1 |                             -- Used to return results of functions
    | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 -- General purpose (caller saved)
    | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 -- General purpose (callee saved)
    | T8 | T9                               -- General purpose (caller saved)
    | K0 | K1                               -- Reserved for OS
    | GP                                    -- Global pointer
    | FP                                    -- Frame pointer
    | SP                                    -- Stack pointer
    | RA                                    -- Record address

data Operand = undefined

data MIPSInstruction =
      Add  Register Register Operand
    | Addi Register Register Constant
    | Sub  Register Register Register
    | Mul  Register Register Register 
    | Div  Register Register Register
    | Rem  Register Register Register
    -- Load instructions
    | La
    | Lw
    | Li
    -- Store instructions
    | Seq Register Register Register
    | Sne Register Register Register
    | Slt Register Register Register 
    | Sgt Register Register Register 
    | Sle Register Register Register 
    | Sge Register Register Register 
    | Sa
    | Sw
    -- Branch instructions
    | Beq  Register Register Label
    | Blt  Register Register Label
    | Bgez Register Label
    -- Jump instructions
    | Jal Register -- Jump to label
    | Jr  Register -- Return from function, resume at address $ra

{-|
    MIPS code generator Monad

    State to carry information about registers allocation
    Writer to store instructions sequentially
    IO to output instruction into a .s file
-}

type MIPSGenerator = RWST MIPSState MIPSReader MIPSWriter IO

MIPSGeneratorState = MIPSGeneratorState 
    { 
    }

initialState :: MIPSGeneratorState
initialState = undefined

TACInstructiontoMIPS :: TAC.Instruction

TACExpressionToMIPS e = case e of
