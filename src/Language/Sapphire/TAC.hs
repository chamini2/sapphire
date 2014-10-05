{-|
    Three-address code (TAC) generation module
-}
module Language.Sapphire.TAC where

import           Language.Sapphire.Program

import           Prelude                   hiding (Ordering(..))
import           Data.Char                 (toLower)

{-|
    Three-address code representation
-}

type Label     = String
type Temporary = String
type Serial    = Int

data Address
    = Name      Identifier
    | Constant  Value
    | Temporary Temporary

instance Show Address where
    show addr = case addr of
        Name idn    -> idn
        Constant v  -> "\\" ++ show v
        Temporary t -> t

data Value
    = ValInt   Int
    | ValFloat Float
    | ValBool  Bool
    | ValChar  Char

instance Show Value where
    show val = case val of
        ValInt   v -> show v
        ValFloat v -> show v
        ValBool  v -> map toLower (show v)
        ValChar  v -> [v]


data Instruction
    = Comment String
    | PutLabel Label String
    | AssignBin
        { result :: Address
        , left   :: Address
        , binop  :: BinOperator
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
    | BeginFunction Int
    | EndFunction   Int
    | PushParameter Address
    | PopParameters
    | Return
    | PCall         Label Int
    | FCall Address Label Int
    -- Goto
    | Goto Label
    | IfGoto      Address Relation Address Label
    | IfTrueGoto  Address Label
    | IfFalseGoto Address Label

instance Show Instruction where
    show ins = case ins of
        Comment str     -> "# " ++ str
        PutLabel la str -> la ++ ":" ++ replicate 10 '\t' ++ "# " ++ str
        _ -> "\t" ++ case ins of
            AssignBin res le o ri -> show res ++ " := " ++ show le ++ " " ++ show o ++ " " ++ show ri
            AssignUn  res o n     -> show res ++ " := " ++ show o  ++ " " ++ show n
            Assign d s            -> show d ++ " := " ++ show s
            BeginFunction nb      -> "begin_function " ++ show nb
            EndFunction nb        -> "end_function " ++ show nb
            PushParameter a       -> "param " ++ show a
            PopParameters         -> "unparam"
            Return                -> "return"
            PCall la i            -> "call " ++ la ++ ", " ++ show i
            FCall d la i          -> show d ++ " := " ++ "call " ++ la ++ ", " ++ show i
            Goto l                -> "goto " ++ show l
            IfGoto le r ri la     -> "if " ++ show le ++ " " ++ show r ++ " " ++ show ri ++ " goto " ++ la
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
    show op = case op of
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
    show op = case op of
        NOT -> "!"
        NEG -> "-"

data Relation
    = EQ | NE
    | LT | LE
    | GT | GE
    deriving (Eq)

instance Show Relation where
    show r = case r of
        EQ -> "=="
        NE -> "/="
        LT -> "<"
        LE -> "<="
        GT -> ">"
        GE -> ">="

binaryToRelation :: Binary -> Relation
binaryToRelation op = case op of
    OpEqual   -> EQ
    OpUnequal -> NE
    OpLess    -> LT
    OpLessEq  -> LE
    OpGreat   -> GT
    OpGreatEq -> GE

binaryToBinOperator :: Binary -> BinOperator
binaryToBinOperator op = case op of
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
    -- OpBelongs ->

unaryToUnOperator :: Unary -> UnOperator
unaryToUnOperator op = case op of
    OpNot    -> NOT
    OpNegate -> NEG
