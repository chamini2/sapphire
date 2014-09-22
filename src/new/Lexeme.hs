module Lexeme
    ( Lexeme(..)
    , fillLex
    ) where

import           Position

data Lexeme a = Lex
    { lexInfo :: a
    , lexPosn :: Position
    } deriving (Eq, Ord)

instance Show a => Show (Lexeme a) where
    show (Lex a p) = show p ++ ": " ++ show a

instance Functor Lexeme where
    fmap f (Lex a p) = Lex (f a) p

----------------------------------------

fillLex :: a -> Lexeme a
fillLex = flip Lex defaultPosn
