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

data Instruction 
    = Comment String
    | Add  Register Register Operand
    {-| Addi Register Register Value-}
    {-| Sub  Register Register Register-}
    {-| Mul  Register Register Register-}
    {-| Div  Register Register Register-}
    {-| Rem  Register Register Register-}
    {--- Load instructions-}
    {-| La Register Operand-}
    {-| Lw-}
    {-| Li-}
    {--- Store instructions-}
    {-| Seq Register Register Register-}
    {-| Sne Register Register Register-}
    {-| Slt Register Register Register-}
    {-| Sgt Register Register Register-}
    {-| Sle Register Register Register-}
    {-| Sge Register Register Register-}
    {-| Sa-}
    {-| Sw-}
    {--- Branch instructions-}
    {-| Beq  Register Register Label-}
    {-| Blt  Register Register Label-}
    {-| Bgez Register Label-}
    {--- Jump instructions-}
    {-| Jal Register -- Jump to label-}
    {-| Jr  Register -- Return from function, resume at address $ra-}

instance Show Instruction where
    show ins = case ins of
        Comment str     -> "# " ++ str
        _               -> "TO DO"
