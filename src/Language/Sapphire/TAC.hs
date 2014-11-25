{-# LANGUAGE LambdaCase #-}
{-|
    Three-address code (TAC) generation module
-}
module Language.Sapphire.TAC
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

import           Language.Sapphire.Program
import           Language.Sapphire.SymbolTable

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

data Location = Address
        { adrName   :: Identifier
        , adrOffset :: Offset
        , adrGlobal :: Global
        }
    deriving (Eq)

instance Show Location where
    show (Address idn a g) = idn ++ "(" ++ show a ++ "|" ++ (if g then "G" else "FP") ++ ")"

data Value
    = ValInt   Int
    | ValFloat Float
    | ValBool  Bool
    | ValChar  Char
    deriving (Eq)

instance Show Value where
    show = \case
        ValInt    v -> show v
        ValFloat  v -> show v
        ValBool   v -> map toLower (show v)
        ValChar   v -> show v

data Instruction
    = Comment       { comment :: String }
    -- Loads
    | LoadConstant  { dst :: Location, val :: Value }
    | LoadLabel     { dst :: Location, label :: Label }
    | Assign        { dst :: Location, src :: Location }
    | Load          { dst :: Location, src :: Location, offset :: Offset }
    | Store         { dst :: Location, src :: Location, offset :: Offset }
    | BinaryOp      { dst :: Location, op  :: BinOp, left :: Location, right :: Location }
    -- Label
    | PutLabel      { label :: Label, comment :: String }
    -- Jumps
    | Goto          { label :: Label }
    | IfTrueGoto    { test :: Location, label :: Label}
    -- Functions
    | BeginFunction { frame :: Width }
    | EndFunction
    | Return        { mSrc :: Maybe Location }
    | PushParam     { src :: Location }
    | PopParams     { bytes :: Width }
    | PCall         { label :: Label }
    | FCall         { label :: Label, dst :: Location }
    -- Print
    | PrintInt      { src :: Location }
    | PrintFloat    { src :: Location }
    | PrintChar     { src :: Location }
    | PrintBool     { src :: Location }
    | PrintString   { label :: Label }

instance Show Instruction where
    show = \case
        Comment com      -> "# " ++ com
        PutLabel lab com -> lab ++ ":" ++ replicate (10 - div (length lab + 1) 4) '\t' ++ "# " ++ str
        instruction -> "\t" ++ case instruction of
            LoadConstant  dst val     -> show dst ++ " := " ++ show val
            LoadLabel     dst lab     -> show dst ++ " := " ++ lab
            Assign        dst src     -> show dst ++ " := " ++ show src
            Load          dst src off -> show dst ++ " := *(" ++ show src ++ (if off /= 0 then " + " ++ show off else []) ++ ")"
            Store         dst src off -> "*(" ++ show dst ++ (if off /= 0 then " + " ++ show off else "") ++ ")"++ " := " ++ show src
            BinaryOp      dst op l r  -> show dst ++ " := " ++ show l ++ " " ++ show op ++ " " ++ show r
            Goto          lab         -> "goto " ++ lab
            IfTrueGoto    tst lab     -> "if " ++ show tst ++ " goto " ++ lab
            BeginFunction frm         -> "function " ++ show frm
            EndFunction               -> "endfunction"
            Return        mSrc        -> "return " ++ fromMaybe "" mSrc
            PushParam     src         -> "param " ++ show src
            PopParams     byt         -> "unparams " ++ show byt
            PCall         lab         -> "call " ++ lab
            FCall         lab dst     -> show dst ++ " := " ++ " call " ++ lab
            PrintInt    ref           -> "printint "    ++ show ref
            PrintFloat  ref           -> "printfloat "  ++ show ref
            PrintChar   ref           -> "printchar "   ++ show ref
            PrintBool   ref           -> "printbool "   ++ show ref
            PrintString lab           -> "printstring " ++ lab
            _  -> error "TAC.Show Instruction: unrecognized instruction"

-- data Instruction
--     = Comment String
--     | PutLabel Label String
--     | AssignBin
--         { result :: Reference
--         , binop  :: BinOperator
--         , left   :: Reference
--         , right  :: Reference
--         }
--     | AssignUn
--         { result  :: Reference
--         , unop    :: UnOperator
--         , operand :: Reference
--         }
--     | Assign
--         { dst :: Reference
--         , src :: Reference
--         }
-- --    | AssignArrR
-- --    | AssignArrL
--     -- Function related instructions
--     | BeginFunction Width
--     | EndFunction
--     | PushParameter Reference
--     | PopParameters Int
--     | Return (Maybe Reference)
--     | PCall           Label Int
--     | FCall Reference Label Int
--     -- Print
--     | PrintInt    Reference
--     | PrintFloat  Reference
--     | PrintChar   Reference
--     | PrintBool   Reference
--     | PrintString Offset    Width
--     -- Read
--     | ReadInt   Reference
--     | ReadFloat Reference
--     | ReadChar  Reference
--     | ReadBool  Reference
--     -- Goto
--     | Goto                                     Label
--     | IfGoto      Relation Reference Reference Label
--     | IfTrueGoto  Reference                    Label
--     | IfFalseGoto Reference                    Label

-- instance Show Instruction where
--     show = \case
--         Comment str     -> "# " ++ str
--         PutLabel lab str -> lab ++ ":" ++ replicate (10 - div (length lab + 1) 4) '\t' ++ "# " ++ str
--         ins -> "\t" ++ case ins of
--             AssignBin res o le ri -> show res ++ " := " ++ show le ++ " " ++ show o ++ " " ++ show ri
--             AssignUn  res o n     -> show res ++ " := " ++ show o  ++ " " ++ show n
--             Assign ds sr          -> show ds ++ " := " ++ show sr
--             BeginFunction by      -> "begin_function " ++ show by
--             EndFunction           -> "end_function"
--             PushParameter ref     -> "param " ++ show ref
--             PopParameters n       -> "popparams " ++ show n
--             Return mayA           -> "return" ++ maybe "" ((" " ++) . show) mayA
--             PCall lab n           -> "call " ++ lab ++ ", " ++ show n
--             FCall ref lab n       -> show ref ++ " := " ++ "call " ++ lab ++ ", " ++ show n
--             PrintInt    ref       -> "print_int "    ++ show ref
--             PrintFloat  ref       -> "print_float "  ++ show ref
--             PrintChar   ref       -> "print_char "   ++ show ref
--             PrintBool   ref       -> "print_bool "   ++ show ref
--             PrintString off wdt   -> "print_string " ++ show off ++ " " ++ show wdt
--             ReadInt   ref         -> "read_int "   ++ show ref
--             ReadFloat ref         -> "read_float " ++ show ref
--             ReadChar  ref         -> "read_char "  ++ show ref
--             ReadBool  ref         -> "read_bool "  ++ show ref
--             Goto lab              -> "goto " ++ lab
--             IfGoto rel le ri lab  -> "if " ++ show le ++ " " ++ show rel ++ " " ++ show ri ++ " goto " ++ lab
--             IfTrueGoto  ref  lab  -> "if "    ++ show ref ++ " goto " ++ lab
--             IfFalseGoto ref  lab  -> "ifnot " ++ show ref ++ " goto " ++ lab
--             _  -> error "TAC.Show Instruction: unrecognized instruction"

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
