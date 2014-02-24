{-# LANGUAGE DeriveDataTypeable #-}
module Language where

import Data.Typeable (typeOf, Typeable(..))

type Program  = [Statement]
type Variable = String

data DataType = Int | Float | Bool | Char | String | Range | Type
    deriving (Show, Typeable)

data Declaration = Declaration Variable DataType
    deriving (Show, Typeable)

data Statement
    = StNoop
    | StAssign Variable Expression

    --Definitions
    | StDeclaration [Declaration]
    | StReturn Expression

    -- I/O
    | StRead  [Variable]
    | StPrint [Expression]

    -- Conditional
    | StIf   ExpressionBool [Statement] [Statement]
    | StCase ExpressionArit [Case] [Statement]

    -- Loops
    | StWhile ExpressionBool [Statement]
    | StFor   Variable ExpressionRang [Statement]
    | StBreak
    | StContinue
    deriving (Show, Typeable)

--data DataType =

data Case = Case Expression [Statement]
    deriving (Show, Typeable)

data Expression
    = ExpressionId Variable
    | ExpressionArit ExpressionArit
    | ExpressionBool ExpressionBool
    | ExpressionStrn ExpressionStrn
    | ExpressionRang ExpressionRang
   -- | ExpressionArry ExpressionArry
    deriving (Show, Typeable)

data ExpressionArit
    = LiteralInt Int
    | LiteralFloat Float
    | PlusArit   ExpressionArit ExpressionArit
    | MinusArit  ExpressionArit ExpressionArit
    | TimesArit  ExpressionArit ExpressionArit
    | DivideArit ExpressionArit ExpressionArit
    | ModuloArit ExpressionArit ExpressionArit
    | PowerArit  ExpressionArit ExpressionArit
    | NegateArit ExpressionArit
    deriving Show

data ExpressionBool
    = LiteralBool Bool
    | OrBool      ExpressionBool ExpressionBool
    | AndBool     ExpressionBool ExpressionBool
    | NotBool     ExpressionBool
    | EqualBool   Expression     Expression
    | UnequalBool Expression     Expression
    | LessBool    ExpressionArit ExpressionArit
    | LessEqBool  ExpressionArit ExpressionArit
    | GreatBool   ExpressionArit ExpressionArit
    | GreatEqBool ExpressionArit ExpressionArit
    | BelongsBool ExpressionArit ExpressionRang
    deriving Show

data ExpressionStrn
    = LiteralStrn String
    deriving Show

data ExpressionRang =
    LiteralRang ExpressionArit ExpressionArit
    deriving Show



-------------------------------------------------------------------------------

class Printer p where
    treePrint :: Int -> p -> String

rep :: Int -> String
rep n = replicate n '\t'

instance (Printer a, Typeable a, Show a) => Printer [a] where
    treePrint n ls
        | ((show $ typeOf ls) == "[Char]") = rep n ++ show ls ++ "\n"
        | otherwise = concat (map (treePrint n) ls)

instance Printer Char where
    treePrint n = show

instance Printer Int where
    treePrint n i = rep n ++ show i ++ "\n"

instance Printer Float where
    treePrint n f = rep n ++ show f ++ "\n"

instance Printer Bool where
    treePrint n b = rep n ++ show b ++ "\n"

instance Printer DataType where
    treePrint n k = rep n ++ show k ++ "\n"

instance Printer Declaration where
    treePrint n (Declaration var typ) = rep n ++ "Declaration" ++ "\n" ++ treePrint (n+1) var ++ treePrint (n+1) typ ++ "\n"

instance Printer Statement where
    treePrint n StNoop = rep n ++ "StNoop" ++ "\n"
    treePrint n (StAssign var expr)           = rep n ++ "StAssign" ++ "\n" ++ treePrint (n+1) var ++ treePrint (n+1) expr ++ "\n"
    treePrint n (StDeclaration decls)         = rep n ++ "StDeclaration" ++ "\n" ++ treePrint (n+1) decls ++ "\n"
    treePrint n (StReturn expr)               = rep n ++ "StReturn" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
    treePrint n (StRead   vars)               = rep n ++ "StRead"   ++ "\n" ++ treePrint (n+1) vars ++ "\n"
    treePrint n (StPrint  exprs)              = rep n ++ "StPrint"  ++ "\n" ++ treePrint (n+1) exprs ++ "\n"
    treePrint n (StIf     bool ifSts elseSts) = rep n ++ "StIf"     ++ "\n" ++ treePrint (n+1) bool ++ rep n ++ "then\n" ++ treePrint (n+1) ifSts ++ rep n ++ "else\n" ++ treePrint (n+1) elseSts ++ "\n"
    treePrint n (StCase   arit cases elseSts) = rep n ++ "StCase"   ++ "\n" ++ treePrint (n+1) arit ++ treePrint (n+1) cases ++ treePrint (n+1) elseSts  ++ "\n"
    treePrint n (StWhile  bool sts)           = rep n ++ "StWhile"  ++ "\n" ++ treePrint (n+1) bool ++ treePrint (n+1) sts ++ "\n"
    treePrint n (StFor    var rang sts)       = rep n ++ "StFor"    ++ "\n" ++ treePrint (n+1) var  ++ treePrint (n+1) rang  ++ treePrint (n+1) sts  ++ "\n"
    treePrint n StBreak    = rep n ++ "StBreak" ++ "\n"
    treePrint n StContinue = rep n ++ "StContinue" ++ "\n"

instance Printer Case where
    treePrint n (Case expr sts) = rep n ++ "Case" ++ "\n" ++ treePrint (n+1) expr ++ treePrint (n+1) sts ++ "\n"

instance Printer Expression where
    treePrint n (ExpressionId var)    = rep n ++ "ExpressionId"   ++ "\n" ++ treePrint (n+1) var ++ "\n"
    treePrint n (ExpressionArit expr) = rep n ++ "ExpressionArit" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
    treePrint n (ExpressionBool expr) = rep n ++ "ExpressionBool" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
    treePrint n (ExpressionStrn expr) = rep n ++ "ExpressionStrn" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
    treePrint n (ExpressionRang expr) = rep n ++ "ExpressionRang" ++ "\n" ++ treePrint (n+1) expr ++ "\n"

instance Printer ExpressionArit where
    treePrint n (LiteralInt num)   = rep n ++ "LiteralInt"      ++ "\n" ++ treePrint (n+1) num ++ "\n"
    treePrint n (LiteralFloat num) = rep n ++ "LiteralFloat"    ++ "\n" ++ treePrint (n+1) num ++ "\n"
    treePrint n (PlusArit   left right) = rep n ++ "PlusArit"   ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (MinusArit  left right) = rep n ++ "MinusArit"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (TimesArit  left right) = rep n ++ "TimesArit"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (DivideArit left right) = rep n ++ "DivideArit" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (ModuloArit left right) = rep n ++ "ModuloArit" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (PowerArit  left right) = rep n ++ "PowerArit"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (NegateArit expr) = rep n ++ "NegateArit"       ++ "\n" ++ treePrint (n+1) expr ++ "\n"

instance Printer ExpressionBool where
    treePrint n (LiteralBool bool) = rep n ++ "LiteralBool" ++ "\n" ++ treePrint (n+1) bool ++ "\n"
    treePrint n (OrBool      left right) = rep n ++ "OrBool"      ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (AndBool     left right) = rep n ++ "AndBool"     ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (NotBool     expr) = rep n ++ "NotBool" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
    treePrint n (EqualBool   left right) = rep n ++ "EqualBool"   ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (UnequalBool left right) = rep n ++ "UnequalBool" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (LessBool    left right) = rep n ++ "LessBool"    ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (LessEqBool  left right) = rep n ++ "LessEqBool"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (GreatBool   left right) = rep n ++ "GreatBool"   ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (GreatEqBool left right) = rep n ++ "GreatEqBool" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
    treePrint n (BelongsBool left right) = rep n ++ "BelongsBool" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"

instance Printer ExpressionStrn where
    treePrint n (LiteralStrn s) = rep n ++ "LiteralStrn" ++ "\n" ++ treePrint (n+1) s ++ "\n"

instance Printer ExpressionRang where
    treePrint n (LiteralRang from to) = rep n ++ "LiteralRang" ++ "\n" ++ treePrint (n+1) from ++ treePrint (n+1) to ++ "\n"























-- This example comes straight from the happy documentation

--data Exp
--      = Let String Exp Exp
--      | Exp1 Exp1
--      deriving Show

--data Exp1
--      = Plus Exp1 Term
--      | Minus Exp1 Term
--      | Term Term
--      deriving Show

--data Term
--      = Times Term Factor
--      | Div Term Factor
--      | Factor Factor
--      deriving Show

--data Factor
--      = Int Int
--      | Var String
--      | Brack Exp
--      deriving Show

--eval :: [(String,Int)] -> Exp -> Int
--eval p (Let var e1 e2) = eval ((var, eval p e1): p) e2
--eval p (Exp1 e)        = evalExp1 p e
--    where
--    evalExp1 p' (Plus  e' t) = evalExp1 p' e' + evalTerm p' t
--    evalExp1 p' (Minus e' t) = evalExp1 p' e' + evalTerm p' t
--    evalExp1 p' (Term  t)    = evalTerm p' t

--    evalTerm p' (Times t f) = evalTerm p' t * evalFactor p' f
--    evalTerm p' (Div   t f) = evalTerm p' t `div` evalFactor p' f
--    evalTerm p' (Factor f)  = evalFactor p' f

--    evalFactor _  (Int i)    = i
--    evalFactor p' (Var s)    = case lookup s p' of
--                               Nothing -> error "free variable"
--                               Just i  -> i
--    evalFactor p' (Brack e') = eval p' e'
