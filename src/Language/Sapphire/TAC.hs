{-# LANGUAGE LambdaCase #-}
{-|
    Three-address code (TAC) generation module
-}
module Language.Sapphire.TAC where

import           Language.Sapphire.Program
import           Language.Sapphire.SymbolTable

import           Data.Char                 (toLower)
import           Prelude                   hiding (Ordering (..))

{-|
    Three-address code representation
-}

type Label     = String
type Temporary = String
type Serial    = Int

data Address
    = Name      Identifier Offset
    | Constant  Value
    | Temporary Temporary

instance Show Address where
    show = \case
        Name idn o  -> idn ++ "(" ++ show o ++ ")"
        Constant v  -> "\\" ++ show v
        Temporary t -> t

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
    | AssignBin
        { result :: Address
        , binop  :: BinOperator
        , left   :: Address
        , right  :: Address
        }
    | AssignUn
        { result  :: Address
        , unop    :: UnOperator
        , operand :: Address
        }
    | Assign
        { dst :: Address
        , src :: Address
        }
--    | AssignArrR
--    | AssignArrL
    -- Function related instructions
    | BeginFunction Width
    | EndFunction
    | PushParameter Address
    | PopParameters Int
    | Return Address
    | PCall         Label Int
    | FCall Address Label Int
    -- Print
    | PrintInt    Address
    | PrintFloat  Address
    | PrintChar   Address
    | PrintBool   Address
    | PrintString Offset  Width
    -- Read
    | ReadInt    Address
    | ReadFloat  Address
    | ReadChar   Address
    | ReadBool   Address
    -- Goto
    | Goto Label
    | IfGoto      Relation Address Address Label
    | IfTrueGoto  Address Label
    | IfFalseGoto Address Label

instance Show Instruction where
    show ins = case ins of
        Comment str     -> "# " ++ str
        PutLabel la str -> la ++ ":" ++ replicate (10 - div (length la + 1) 4) '\t' ++ "# " ++ str
        _ -> "\t" ++ case ins of
            AssignBin res o le ri -> show res ++ " := " ++ show le ++ " " ++ show o ++ " " ++ show ri
            AssignUn  res o n     -> show res ++ " := " ++ show o  ++ " " ++ show n
            Assign d s            -> show d ++ " := " ++ show s
            BeginFunction by      -> "begin_function " ++ show by
            EndFunction           -> "end_function"
            PushParameter a       -> "param " ++ show a
            PopParameters n       -> "unparam " ++ show n
            Return a              -> "return " ++ show a
            PCall la i            -> "call " ++ la ++ ", " ++ show i
            FCall d la i          -> show d ++ " := " ++ "call " ++ la ++ ", " ++ show i
            PrintInt    a         -> "print_int "    ++ show a
            PrintFloat  a         -> "print_float "  ++ show a
            PrintChar   a         -> "print_char "   ++ show a
            PrintBool   a         -> "print_bool "   ++ show a
            PrintString o w       -> "print_string " ++ show o ++ " " ++ show w
            ReadInt    a          -> "read_int "   ++ show a
            ReadFloat  a          -> "read_float " ++ show a
            ReadChar   a          -> "read_char "  ++ show a
            ReadBool   a          -> "read_bool "  ++ show a
            Goto la               -> "goto " ++ la
            IfGoto r le ri la     -> "if " ++ show le ++ " " ++ show r ++ " " ++ show ri ++ " goto " ++ la
            IfTrueGoto  a la      -> "if "    ++ show a ++ " goto " ++ la
            IfFalseGoto a la      -> "ifnot " ++ show a ++ " goto " ++ la

data BinOperator
    = ADD  | SUB | MUL | DIV | MOD | POW
    | OR   | AND
    | ArrR | ArrL
    | Rel Relation
    deriving (Eq)

data UnOperator
    = NOT | NEG

instance Show BinOperator where
    show = \case
        ADD   -> "+"
        SUB   -> "-"
        MUL   -> "*"
        DIV   -> "/"
        MOD   -> "%"
        POW   -> "^"
        OR    -> "||"
        AND   -> "&&"
        ArrR  -> "=[]"
        ArrL  -> "[]="
        Rel r -> show r

instance Show UnOperator where
    show = \case
        NOT -> "!"
        NEG -> "-"

data Relation
    = EQ | NE
    | LT | LE
    | GT | GE
    deriving (Eq)

instance Show Relation where
    show = \case
        EQ -> "=="
        NE -> "/="
        LT -> "<"
        LE -> "<="
        GT -> ">"
        GE -> ">="

binaryToRelation :: Binary -> Relation
binaryToRelation = \case
    OpEqual   -> EQ
    OpUnequal -> NE
    OpLess    -> LT
    OpLessEq  -> LE
    OpGreat   -> GT
    OpGreatEq -> GE
    _         -> error "TAC.binaryToRelation: trying to convert a non-relation binary operator to a intermediate code relation operator"

binaryToBinOperator :: Binary -> BinOperator
binaryToBinOperator = \case
    OpPlus    -> ADD
    OpMinus   -> SUB
    OpTimes   -> MUL
    OpDivide  -> DIV
    OpModulo  -> MOD
    OpPower   -> POW
    -- OpFromTo  ->
    OpOr      -> OR
    OpAnd     -> AND
    OpEqual   -> Rel EQ
    OpUnequal -> Rel NE
    OpLess    -> Rel LT
    OpLessEq  -> Rel LE
    OpGreat   -> Rel GT
    OpGreatEq -> Rel GE
    _         -> error "TAC.binaryToBinOperator: trying to convert '@' or '..' to a intermediate code binary operator"
    -- OpBelongs ->

unaryToUnOperator :: Unary -> UnOperator
unaryToUnOperator = \case
    OpNot    -> NOT
    OpNegate -> NEG
