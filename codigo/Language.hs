{-# LANGUAGE DeriveDataTypeable #-}
module Language where

import Data.Typeable (Typeable(..), typeOf)

data Statement
    = NoOp
    | Assign VarName Expression
    deriving Show

data Expression
    = ExpressionArit ExpressionArit
    | ExpressionBool ExpressionBool
    | ExpressionStrn ExpressionStrn
    deriving Show

data ExpressionArit
    = LiteralInt Int
    | LiteralFloat Float
    | PlusArit ExpressionArit ExpressionArit
    deriving Show

data ExpressionBool
    = LiteralBool Bool
    | OrBool ExpressionBool ExpressionBool
    | AndBool ExpressionBool ExpressionBool
    | NotBool ExpressionBool
    deriving Show

data ExpressionStrn
    = LiteralStrn String
    deriving Show


























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


