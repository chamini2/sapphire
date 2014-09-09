{-# LANGUAGE DeriveDataTypeable #-}
module Lexeme
    ( Lexeme(..) ) where

import           Position
import qualified Data.Data              as DD (Data)
import qualified Data.Typeable          as DT (Typeable)

data Lexeme a = Lex
    { lexInfo :: a
    , lexPosn :: Position
    } deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show a => Show (Lexeme a) where
    show (Lex a p) = case p of
        -- Everything in "(0,0)" shouldn't be shown
        --Posn (0,0) -> ""
        _          -> show p ++ ": " ++ show a

instance Functor Lexeme where
    fmap f (Lex a p) = Lex (f a) p
