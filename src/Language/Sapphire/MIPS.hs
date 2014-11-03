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
    {-, Operand-}
    , Instruction(..)
    ) where

import           Data.Char            (toLower)
import           Data.Data
import           Data.Typeable

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
    | Constant         Value 
    | Indexed          Value Register
    | IndirectRegister Register
    | IndirectIndexed  Value Register

data Value
    = ValInt    Int
    | ValFloat  Float
    | ValBool   Bool
    | ValChar   Char
    | ValString String

instance Show Value where
    show = \case
        ValInt    v -> show v
        ValFloat  v -> show v
        ValBool   v -> map toLower (show v)
        ValChar   v -> [v]
        ValString v -> v

data Instruction 
    = Comment String
    | PutLabel Label String 
    -- Arithmetic
    | Add  Register Register Register
    | Addi Register Register Value
    | Sub  Register Register Register
    | Mul  Register Register Register
    | Div  Register Register
    {-| Rem  Register Register Register-}
    -- Load instructions
    | La Register Label
    | Li Register Value
    | Lw Register Label Register 
    | Mflo Register
    | Mfhi Register
    | Mtlo Register
    | Mthi Register
    -- Store instructions
    | Sw  Register Label Register 
    | Slt Register Register Register
    -- Branch instructions
    | B    Label
    | Beq  Register Register Label
    | Beqz Register Label
    | Bgez Register Label
    | Bgtz Register Label
    | Blez Register Label
    | Bltz Register Label
    | Bne  Register Register Label
    | Bnez Register Label
    -- Jump instructions
    | J   Label    -- Jump to label
    | Jal Label    -- Jump to label
    | Jr  Register -- Return from function, resume at address $ra

instance Show Instruction where
    show = \case
        Comment str     -> "# " ++ str
        PutLabel lab str -> lab ++ ":" ++ replicate (10 - div (length lab + 1) 4) '\t' ++ "# " ++ str
        ins -> "\t" ++ case ins of
            Add  rd rs rt -> "add\t\t"  ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Addi rd rs rt -> "addi\t\t" ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Sub  rd rs rt -> "sub\t\t"  ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Mul  rd rs rt -> "mul\t\t"  ++ show rd ++ ", " ++ show rs ++ ", " ++ show rt 
            Div  rs rt    -> "div\t\t"  ++ show rs ++ ", " ++ show rt 
            -- Load instructions
            La rd lab     -> "la\t\t"   ++ show rd ++ ", " ++ lab
            Mflo rd       -> "mflo\t\t" ++ show rd
            Mfhi rd       -> "mfhi\t\t" ++ show rd
            Mtlo rs       -> "mtlo\t\t" ++ show rs
            Mthi rs       -> "mthi\t\t" ++ show rs
            -- Store instructions
            Sw  rs lab rt -> " sw\t\t"  ++ show rs ++ ", " ++ lab ++ "(" ++ show rt ++ ")"
            _             -> undefined
            {-Slt Register Register Register-}
            {--- Branch instructions-}
            {-B    Label-}
            {-Beq  Register Register Label-}
            {-Beqz Register Label-}
            {-Bgez Register Label-}
            {-Bgtz Register Label-}
            {-Blez Register Label-}
            {-Bltz Register Label-}
            {-Bne  Register Register Label-}
            {-Bnez Register Label-}
            {--- Jump instructions-}
            {-J   Label    -- Jump to label-}
            {-Jal Label    -- Jump to label-}
            {-Jr  Register -- Return from function, resume at address $ra-}

