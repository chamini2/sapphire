module Language.Sapphire.Lexeme
    ( Lexeme(..)
    , pure

    -- From Position
    , Position(..)
    , defaultPosn
    , row
    , col
    ) where

import           Language.Sapphire.Position

import           Control.Applicative        (Applicative, pure, (<*>))

data Lexeme a = Lex
    { lexInfo :: a
    , lexPosn :: Position
    } deriving (Eq, Ord)

instance Show a => Show (Lexeme a) where
    show (Lex a p) = show p ++ ": " ++ show a

instance Functor Lexeme where
    fmap f (Lex a p) = Lex (f a) p

instance Applicative Lexeme where
    pure = fillLex
    -- We keep the function's position. #decisions
    (Lex f p) <*> (Lex a _) = Lex (f a) p

----------------------------------------

fillLex :: a -> Lexeme a
fillLex = flip Lex defaultPosn
