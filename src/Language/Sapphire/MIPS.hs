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
    | Label            Label
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
    -- Data declarations
    | Asciiz Label String
    {-| Word ???-}
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
    | La Register Operand     -- Load address
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
        PutLabel lab str -> lab ++ (tabs $ length lab) ++ (if str /= "" then ("# " ++ str) else "")
        ins -> "\t" ++ case ins of
            --  Data declarations
            Asciiz lab str  -> lab ++ ": .asciiz " ++ str 
            --  Arithmetic
            Add   rd rs rt  -> "add"   ++ (tabs 3) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Addi  rd rs imm -> "addi"  ++ (tabs 4) ++ show rd ++ ", " ++ show rs ++ ", " ++ show imm 
            Addiu rd rs imm -> "addiu" ++ (tabs 5) ++ show rd ++ ", " ++ show rs ++ ", " ++ show imm 
            Sub   rd rs rt  -> "sub"   ++ (tabs 3) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Subu  rd rs imm -> "subu"  ++ (tabs 4) ++ show rd ++ ", " ++ show rs ++ ", " ++ show imm 
            Mul   rd rs rt  -> "mul"   ++ (tabs 3) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Mult     rs rt  -> "mult"  ++ (tabs 4) ++ show rs ++ ", " ++ show rt 
            Div      rs rt  -> "div"   ++ (tabs 3) ++ show rs ++ ", " ++ show rt 
            --  Boolean 
            Or  rd rs rt -> "or"  ++ (tabs 2) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt
            And rd rs rt -> "and" ++ (tabs 3) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt
            --  Move
            Move rd rs    -> "move" ++ (tabs 4) ++ show rd ++ ", " ++ show rs
            --  Load instructions
            La rd ind     -> "la"   ++ (tabs 2) ++ show rd ++ ", " ++ show ind
            Li rd imm     -> "li"   ++ (tabs 2) ++ show rd ++ ", " ++ show imm
            Lw rd ind     -> "lw"   ++ (tabs 2) ++ show rd ++ ", " ++ show ind
            Ld rd ind     -> "ld"   ++ (tabs 2) ++ show rd ++ ", " ++ show ind
            Mflo rd       -> "mflo" ++ (tabs 4) ++ show rd
            Mfhi rd       -> "mfhi" ++ (tabs 4) ++ show rd
            Mtlo rs       -> "mtlo" ++ (tabs 4) ++ show rs
            Mthi rs       -> "mthi" ++ (tabs 4) ++ show rs
            --  Store instructions
            Sw  rs ind    -> "sw"  ++ (tabs 2) ++ show rs ++ ", " ++ show ind
            Slt rd rs  rt -> "slt" ++ (tabs 3) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Seq rd rs  rt -> "seq" ++ (tabs 3) ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            --  Branch instructions
            B          lab -> "b"    ++ (tabs 1) ++ lab
            Beq  rs rt lab -> "beq"  ++ (tabs 3) ++ show rs ++ ", " ++ show rt ++ ", " ++ lab
            Beqz rs    lab -> "beqz" ++ (tabs 4) ++ show rs ++ ", " ++ lab
            Bgez rs    lab -> "bgez" ++ (tabs 4) ++ show rs ++ ", " ++ lab
            Bgtz rs    lab -> "bgtz" ++ (tabs 4) ++ show rs ++ ", " ++ lab
            Blez rs    lab -> "blez" ++ (tabs 4) ++ show rs ++ ", " ++ lab
            Bltz rs    lab -> "bltz" ++ (tabs 4) ++ show rs ++ ", " ++ lab
            Bne  rs rt lab -> "bne"  ++ (tabs 3) ++ show rs ++ ", " ++ show rt ++ ", " ++ lab
            Bnez rs    lab -> "bnez" ++ (tabs 4) ++ show rs ++ ", " ++ lab
            --  Jump instructions
            J   lab -> "j"    ++ (tabs 1) ++ lab    
            Jal lab -> "jal"  ++ (tabs 3) ++ lab    
            Jalr r  -> "jalr" ++ (tabs 4) ++ show r
            Jr  rs  -> "jr"   ++ (tabs 2) ++ show rs    
            --  Other instructions
            Nop     -> "nop"
            Break   -> "break"
            Syscall -> "syscall"
            _  -> error "MIPS.Show Instruction: unrecognized instruction"
        where tabs l = replicate (5 - div (l + 1) 4) '\t'
