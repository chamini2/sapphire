{-# LANGUAGE LambdaCase #-}
{-|
    Three-address code (TAC) generation module
-}
module Language.Sapphire.TAC
    ( Location(..)
    , Value(..)

    , TAC
    , Label
    , Serial
    , Global
    , Base

    , Instruction(..)
    , BinOperator(..)
    , Relation(..)

    , binaryToRelation
    , binaryToBinOperator
    , hasGoto
    , isPutLabel
    , isComment
    ) where

import           Language.Sapphire.Program
import           Language.Sapphire.SymbolTable

import           Data.Char                     (toLower)
import           Data.Maybe                    (fromMaybe)
import           Data.Sequence                 (Seq)
import           Prelude                       hiding (Ordering (..))

{-|
    Three-address code representation
-}

type TAC = Seq Instruction

type Label  = String
type Serial = Int
type Global = Bool
type Base   = Int

data Location = Address
        { addrName   :: Identifier
        , addrOffset :: Offset
        , addrGlobal :: Global
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
        ValInt   v -> show v
        ValFloat v -> show v
        ValBool  v -> map toLower (show v)
        ValChar  v -> show v

data Instruction
    = Comment       { comment :: String }
    -- Label
    | PutLabel      { label :: Label }
    -- Loads
    | LoadConstant  { destin :: Location, value :: Value }
    | Load          { destin :: Location, base  :: Base, indirect :: Location }
    | Store         { source :: Location, base  :: Base, indirect :: Location }
    | BinaryOp      { destin :: Location, binop :: BinOperator, left :: Location, right :: Location }
    -- Jumps
    | Goto          { label :: Label }
    | IfTrueGoto    { test  :: Location, label :: Label}
    -- Functions
    | BeginFunction { frame :: Width }
    | EndFunction
    | Return        { maysource :: Maybe Location }
    | PushParam     { source    :: Location }
    | PopParams     { bytes  :: Width }
    | PCall         { label  :: Label }
    | FCall         { label  :: Label, destin :: Location }
    -- Print
    | PrintInt      { source   :: Location }
    | PrintFloat    { source   :: Location }
    | PrintChar     { source   :: Location }
    | PrintBool     { source   :: Location }
    | PrintString   { label :: Label }
    -- Read
    | ReadInt       { destin :: Location }
    | ReadFloat     { destin :: Location }
    | ReadChar      { destin :: Location }
    | ReadBool      { destin :: Location }

instance Show Instruction where
    show = \case
        Comment com  -> "# " ++ com
        -- Label
        PutLabel lab -> lab ++ ":"
        instruction -> "\t" ++ case instruction of
            -- Loads
            LoadConstant  d v      -> show d ++ " := " ++ show v
            Load          d b ind  -> show d ++ " := *(" ++ show ind ++ (if b /= 0 then " + " ++ show b else []) ++ ")"
            Store         s b ind  -> "*(" ++ show ind ++ (if b /= 0 then " + " ++ show b else "") ++ ")"++ " := " ++ show s
            BinaryOp      d op l r -> show d ++ " := " ++ show l ++ " " ++ show op ++ " " ++ show r
            -- Jumps
            Goto          lab     -> "goto " ++ lab
            IfTrueGoto    tst lab -> "if " ++ show tst ++ " goto " ++ lab
            -- Functions
            BeginFunction frm   -> "function " ++ show frm
            EndFunction         -> "endfunction"
            Return        mSrc  -> "return " ++ fromMaybe "" (fmap show mSrc)
            PushParam     s     -> "param " ++ show s
            PopParams     byt   -> "unparams " ++ show byt
            PCall         lab   -> "call " ++ lab
            FCall         lab d -> show d ++ " := " ++ " call " ++ lab
            -- Print
            PrintInt      s   -> "printint "    ++ show s
            PrintFloat    s   -> "printfloat "  ++ show s
            PrintChar     s   -> "printchar "   ++ show s
            PrintBool     s   -> "printbool "   ++ show s
            PrintString   lab -> "printstring " ++ lab
            -- Read
            ReadInt       d -> "readint "   ++ show d
            ReadFloat     d -> "readfloat " ++ show d
            ReadChar      d -> "readchar "  ++ show d
            ReadBool      d -> "readbool "  ++ show d
            _  -> error "TAC.Show Instruction: unrecognized instruction"

data BinOperator
    = ADD  | SUB | MUL | DIV | MOD | POW
    | OR   | AND | XOR
    | Rel Relation
    deriving (Eq)

data Relation
    = EQ | NE
    | LT | LE
    | GT | GE
    deriving (Eq)

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
        XOR   -> "~"
        -- ArrR  -> "=[]"
        -- ArrL  -> "[]="
        Rel r -> show r

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
    OpOr      -> OR
    OpAnd     -> AND
    OpEqual   -> Rel EQ
    OpUnequal -> Rel NE
    OpLess    -> Rel LT
    OpLessEq  -> Rel LE
    OpGreat   -> Rel GT
    OpGreatEq -> Rel GE
    _         -> error "TAC.binaryToBinOperator: trying to convert '@' or '..' to a intermediate code binary operator"
    -- OpFromTo
    -- OpBelongs

hasGoto :: Instruction -> Bool
hasGoto = \case
    Goto         _ -> True
    IfTrueGoto _ _ -> True
    _              -> False

isPutLabel :: Instruction -> Bool
isPutLabel = \case
    PutLabel _ -> True
    _          -> False

isComment :: Instruction -> Bool
isComment = \case
    Comment _ -> True
    _         -> False
