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
        Constant v  -> "!" ++ show v
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
    | PutLabel Label
    | Assign            -- Quadruples
        { result :: Address
        , opr    :: Operator
        , left   :: Address
        , right  :: Address
        }
--    | AssignBin
--    | AssignUn
--    | AssignArrR
--    | AssignArrL
    -- Function related instructions
    | BeginFunction Int
    | EndFunction   Int
    | PushParameter Address
    | PopParameters
    | Return
    | LCall
    | ACall
    -- Goto
    | Goto Label
    | IfGoto Address Relation Address Label
    -- Move
    | Move
        { dst :: Address
        , src :: Address
        }

instance Show Instruction where
    show ins = case ins of
        PutLabel la -> la ++ ": "
        Comment str -> "# " ++ str
        _ -> "\t" ++ case ins of
            Assign res o le ri -> show res ++ " := " ++ show le ++ " " ++ show o ++ " " ++ show ri
            BeginFunction nb   -> "begin_function " ++ show nb
            EndFunction nb     -> "end_function " ++ show nb
            PushParameter a    -> "param " ++ show a
            PopParameters      -> ""
            Return             -> ""
            LCall              -> ""
            ACall              -> ""
            Goto l             -> "goto " ++ show l
            IfGoto le r ri la  -> "if " ++ show le ++ " " ++ show r ++ " " ++ show ri ++ " goto " ++ la
            Move d s           -> show d ++ " := " ++ show s


data Operator
    = ADD  | SUB | MUL | DIV | MOD | POW
    | OR   | AND
    | ArrR | ArrL
    | Rel Relation
    deriving (Eq)

instance Show Operator where
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

binaryToOperator :: Binary -> Operator
binaryToOperator op = case op of
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
