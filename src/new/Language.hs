{-# LANGUAGE DeriveDataTypeable #-}
module Language where

import           Control.Monad.Identity hiding (forM_, mapM_)
import           Control.Monad.State    hiding (forM_, mapM_)
import           Control.Monad.Writer   hiding (forM_, mapM_)
import           Data.Char              (toLower)
import qualified Data.Data              as DD
import           Data.Foldable          as DF (concat, concatMap, foldr, forM_,
                                               mapM_, toList)
import           Data.Function          (on)
import           Data.Functor           ((<$))
import           Data.List              (intercalate)
import           Data.Maybe             (fromJust)
import           Data.Sequence          as DS (Seq, fromList, singleton)
import qualified Data.Typeable          as DT
import           Prelude                hiding (concat, concatMap, mapM_)

-- Posn (Row, Column)
newtype Position = Posn (Int, Int)
    deriving (Bounded, Eq, DD.Data, Ord, Read, DT.Typeable)

instance Show Position where
    show (Posn (row, col)) = show row ++ "," ++ show col

----------------------------------------

data Lexeme a = Lex
    { lexInfo :: a
    , lexPosn :: Position
    } deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show a => Show (Lexeme a) where
    show (Lex a p) = case p of
        (0,0) -> ""
        -     -> show p ++ ": " ++ show a

instance Functor Lexeme where
    fmap f (Lex a p) = Lex (f a) p

--------------------------------------------------------------------------------
