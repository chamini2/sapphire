{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
    MIPS related data structures
-}
module Language.Sapphire.MIPS
    ( Label
    , Register(..)
    , Value
    {-, FloatRegister-}
    , Operand(..)
    , Instruction(..)
    ) where

import           Data.Char            (toLower)
import           Data.Data

{-|
    MIPS related information and data types
-}

type Label = String

{-|
    MIPS 32 registers
 -}
data Register 
    = Zero                                  -- Constant register
--  | AT                                    -- Reserved for assembler
    | A0 | A1 | A2 | A3                     -- Used to pass first four arguments to function call
    | V0 | V1                               -- Used to return results of functions
    | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 -- General purpose (caller saved)
    | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 -- General purpose (callee saved)
    | T8 | T9                               -- General purpose (caller saved)
--  | K0 | K1                               -- Reserved for OS
    | GP                                    -- Global pointer
    | FP                                    -- Frame pointer
    | SP                                    -- Stack pointer
    | RA                                    -- Record address
    deriving (Eq, Typeable, Data)

instance Show Register where
    show reg = "$" ++ (map toLower) (showConstr $ toConstr reg)

{-|
    MIPS 32 floating-point registers
-}
{-data FloatRegister =-}
    {-= Zero                                  -- Constant register-}
    {-| AT                                    -- Reserved for assembler-}
    {-| A0 | A1 | A2 | A3                     -- Used to pass first four arguments to function call-}
    {-| V0 | V1                               -- Used to return results of functions-}
    {-| T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 -- General purpose (caller saved)-}
    {-| S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 -- General purpose (callee saved)-}
    {-| T8 | T9                               -- General purpose (caller saved)-}
    {-| K0 | K1                               -- Reserved for OS-}
    {-| GP                                    -- Global pointer-}
    {-| FP                                    -- Frame pointer-}
    {-| SP                                    -- Stack pointer-}
    {-| RA                                    -- Record address-}

data Operand 
    = Register         Register
    | Const            Int 
    | Indexed          Int Register
    {-| IndirectRegister Register-}
    {-| IndirectIndexed  Value Register-}

instance Show Operand where
    show = \case
        Register reg    -> show reg
        Const int       -> show int
        Indexed int reg -> show int ++ "(" ++ show reg ++ ")"

data Value
    = ValInt    Int
    {-| ValFloat  Float-}
    {-| ValBool   Bool-}
    | ValChar   Char
    | ValString String

instance Show Value where
    show = \case
        ValInt    v -> show v
        {-ValFloat  v -> show v-}
        {-ValBool   v -> map toLower (show v)-}
        ValChar   v -> [v]
        ValString v -> v

data Instruction 
    = Comment String
    | PutLabel Label String 
    -- Arithmetic
    | Add   Register Register Register
    | Addi  Register Register Operand
    | Addiu Register Register Operand
    | Sub   Register Register Register
    | Subu  Register Register Operand
    | Mul   Register Register Register
    | Mult  Register Register 
    | Div   Register Register
    -- Boolean operations
    | Or  Register Register Register
    | And Register Register Register
    -- Move
    | Move Register Register
    -- Load instructions
    | La Register Label     -- Load address
    | Li Register Operand   -- Load immediate
    | Lw Register Operand   -- Load word
    | Ld Register Operand   -- Load double word
    | Mflo Register
    | Mfhi Register
    | Mtlo Register
    | Mthi Register
    -- Store instructions
    | Sw  Register Operand  
    | Slt Register Register Register        --  Set Rd to 1 if Rs < Rt, 0 otherwise
    | Seq Register Register Register        --  Set Rd to 1 if Rs == Rt, 0 otherwise
    -- Branch instructions
    | B    Label                            --  Unconditional branch
    | Beq  Register Register Label          --  Branch if registers hold the same value
    | Beqz Register Label                   --  Branch to Label if Register equals zero
    | Bgez Register Label
    | Bgtz Register Label
    | Blez Register Label
    | Bltz Register Label
    | Bne  Register Register Label
    | Bnez Register Label
    -- Jump instructions
    | J    Label    -- Jump to label
    | Jal  Label    -- Jump to label (save $ra)
    | Jalr Register -- Jump to label at register (save $ra)
    | Jr   Register -- Return from function, resume at address $ra
    -- Other instructions
    | Nop
    | Break
    | Syscall

instance Show Instruction where
    show = \case
        Comment str      -> "# " ++ str
        PutLabel lab str -> lab ++ ":" ++ replicate (10 - div (length lab + 1) 4) '\t' ++ "# " ++ str
        ins -> "\t" ++ case ins of
            --  Arithmetic
            Add   rd rs rt  -> "add\t\t"   ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Addi  rd rs imm -> "addi\t\t"  ++ show rd ++ ", " ++ show rs ++ ", " ++ show imm 
            Addiu rd rs imm -> "addiu\t\t" ++ show rd ++ ", " ++ show rs ++ ", " ++ show imm 
            Sub   rd rs rt  -> "sub\t\t"   ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Subu  rd rs imm -> "subu\t\t"  ++ show rd ++ ", " ++ show rs ++ ", " ++ show imm 
            Mul   rd rs rt  -> "mul\t\t"   ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Mult     rs rt  -> "mult\t\t"  ++ show rs ++ ", " ++ show rt 
            Div      rs rt  -> "div\t\t"   ++ show rs ++ ", " ++ show rt 
            --  Boolean 
            Or  rd rs rt -> "or "  ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt
            And rd rs rt -> "and " ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt
            --  Move
            Move rd rs    -> "move " ++ show rd ++ ", " ++ show rs
            --  Load instructions
            La rd lab     -> "la\t\t"   ++ show rd ++ ", " ++ lab
            Li rd imm     -> "li\t\t"   ++ show rd ++ ", " ++ show imm
            Lw rd ind     -> "lw\t\t"   ++ show rd ++ ", " ++ show ind
            Ld rd ind     -> "ld\t\t"   ++ show rd ++ ", " ++ show ind
            Mflo rd       -> "mflo\t\t" ++ show rd
            Mfhi rd       -> "mfhi\t\t" ++ show rd
            Mtlo rs       -> "mtlo\t\t" ++ show rs
            Mthi rs       -> "mthi\t\t" ++ show rs
            --  Store instructions
            Sw  rs ind    -> "sw\t\t"  ++ show rs ++ ", " ++ show ind
            Slt rd rs  rt -> "slt\t\t" ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Seq rd rs  rt -> "seq\t\t" ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            --  Branch instructions
            B          lab -> "b\t\t"    ++ lab
            Beq  rs rt lab -> "beq\t\t"  ++ show rs ++ ", " ++ show rt ++ ", " ++ lab
            Beqz rs    lab -> "beqz\t\t" ++ show rs ++ ", " ++ lab
            Bgez rs    lab -> "bgez\t\t" ++ show rs ++ ", " ++ lab
            Bgtz rs    lab -> "bgtz\t\t" ++ show rs ++ ", " ++ lab
            Blez rs    lab -> "blez\t\t" ++ show rs ++ ", " ++ lab
            Bltz rs    lab -> "bltz\t\t" ++ show rs ++ ", " ++ lab
            Bne  rs rt lab -> "bne\t\t"  ++ show rs ++ ", " ++ show rt ++ ", " ++ lab
            Bnez rs    lab -> "bnez\t\t" ++ show rs ++ ", " ++ lab
            --  Jump instructions
            J   lab -> "j\t\t"    ++ lab    
            Jal lab -> "jal\t\t"  ++ lab    
            Jalr r  -> "jalr\t\t" ++ show r
            Jr  rs  -> "jr\t\t"   ++ show rs    
            --  Other instructions
            Nop     -> "nop"
            Break   -> "break"
            Syscall -> "syscall"
            _  -> error "MIPS.Show Instruction: unrecognized instruction"
