{-# LANGUAGE LambdaCase #-}
{-|
    Three-address code (TAC) generation module
-}
module Language.Safiro.TAC
    ( Reference(..)
    , Value(..)

    , TAC
    , Label
    , Serial
    , Global

    , Instruction(..)
    , BinOperator(..)
    , UnOperator(..)
    , Relation(..)

    , binaryToRelation
    , binaryToBinOperator
    , unaryToUnOperator
    , hasGoto
    , getLabel
    , isPutLabel
    ) where

import           Language.Safiro.Program
import           Language.Safiro.SymbolTable

import           Data.Char                     (toLower)
import           Data.Sequence                 (Seq)
import           Prelude                       hiding (Ordering (..))

{-|
    Three-address code representation
-}

type TAC = Seq Instruction

type Label  = String
type Serial = Int
type Global = Bool

data Reference
    = Address   Identifier Reference Global
    | Constant  Value
    | Temporary Serial
    deriving (Eq)

instance Show Reference where
    show = \case
        Address idn a g -> idn ++ "(" ++ show a ++ "|" ++ (if g then "G" else "FP") ++ ")"
        Constant v      -> "\\" ++ show v
        Temporary s     -> "$T" ++ show s

data Value
    = ValInt    Int
    | ValFloat  Float
    | ValBool   Bool
    | ValChar   Char
    | ValString String
    deriving (Eq)

instance Show Value where
    show = \case
        ValInt    v -> show v
        ValFloat  v -> show v
        ValBool   v -> map toLower (show v)
        ValChar   v -> show v
        ValString v -> v

data Instruction
    = Comment String
    | PutLabel Label String
    | AssignBin
        { result :: Reference
        , binop  :: BinOperator
        , left   :: Reference
        , right  :: Reference
        }
    | AssignUn
        { result  :: Reference
        , unop    :: UnOperator
        , operand :: Reference
        }
    | Assign
        { dst :: Reference
        , src :: Reference
        }
--    | AssignArrR
--    | AssignArrL
    -- Function related instructions
    | BeginFunction Width
    | EndFunction
    | PushParameter Reference
    | PopParameters Int
    | Return (Maybe Reference)
    | PCall           Label Int
    | FCall Reference Label Int
    -- Print
    | PrintInt    Reference
    | PrintFloat  Reference
    | PrintChar   Reference
    | PrintBool   Reference
    | PrintString Offset    Width
    -- Read
    | ReadInt   Reference
    | ReadFloat Reference
    | ReadChar  Reference
    | ReadBool  Reference
    -- Goto
    | Goto                                     Label
    | IfGoto      Relation Reference Reference Label
    | IfTrueGoto  Reference                    Label
    | IfFalseGoto Reference                    Label

instance Show Instruction where
    show = \case
        Comment str     -> "# " ++ str
        PutLabel lab str -> lab ++ ":" ++ replicate (10 - div (length lab + 1) 4) '\t' ++ "# " ++ str
        ins -> "\t" ++ case ins of
            AssignBin res o le ri -> show res ++ " := " ++ show le ++ " " ++ show o ++ " " ++ show ri
            AssignUn  res o n     -> show res ++ " := " ++ show o  ++ " " ++ show n
            Assign ds sr          -> show ds ++ " := " ++ show sr
            BeginFunction by      -> "begin_function " ++ show by
            EndFunction           -> "end_function"
            PushParameter ref     -> "param " ++ show ref
            PopParameters n       -> "popparams " ++ show n
            Return mayA           -> "return" ++ maybe "" ((" " ++) . show) mayA
            PCall lab n           -> "call " ++ lab ++ ", " ++ show n
            FCall ref lab n       -> show ref ++ " := " ++ "call " ++ lab ++ ", " ++ show n
            PrintInt    ref       -> "print_int "    ++ show ref
            PrintFloat  ref       -> "print_float "  ++ show ref
            PrintChar   ref       -> "print_char "   ++ show ref
            PrintBool   ref       -> "print_bool "   ++ show ref
            PrintString off wdt   -> "print_string " ++ show off ++ " " ++ show wdt
            ReadInt   ref         -> "read_int "   ++ show ref
            ReadFloat ref         -> "read_float " ++ show ref
            ReadChar  ref         -> "read_char "  ++ show ref
            ReadBool  ref         -> "read_bool "  ++ show ref
            Goto lab              -> "goto " ++ lab
            IfGoto rel le ri lab  -> "if " ++ show le ++ " " ++ show rel ++ " " ++ show ri ++ " goto " ++ lab
            IfTrueGoto  ref  lab  -> "if "    ++ show ref ++ " goto " ++ lab
            IfFalseGoto ref  lab  -> "ifnot " ++ show ref ++ " goto " ++ lab
            _  -> error "TAC.Show Instruction: unrecognized instruction"

data BinOperator
    = ADD  | SUB | MUL | DIV | MOD | POW
    | OR   | AND
--    | ArrR | ArrL
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
        OR    -> "|"
        AND   -> "&"
        -- ArrR  -> "=[]"
        -- ArrL  -> "[]="
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

hasGoto :: Instruction -> Bool
hasGoto = \case
    Goto              _ -> True
    IfGoto      _ _ _ _ -> True
    IfTrueGoto  _     _ -> True
    IfFalseGoto _     _ -> True
    _                   -> False

getLabel :: Instruction -> Label
getLabel = \case
    PutLabel        l _ -> l
    Goto              l -> l
    IfGoto      _ _ _ l -> l
    IfTrueGoto  _     l -> l
    IfFalseGoto _     l -> l
    _                   -> error "TAC.getLabel: getting label from non-labeled Instruction"

isPutLabel :: Instruction -> Bool
isPutLabel = \case
    PutLabel _ _ -> True
    _            -> False
