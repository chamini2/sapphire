{-# LANGUAGE DeriveDataTypeable #-}
module Position
    ( Position(..)
    , defaultPosn
    ) where

import qualified Data.Data     as DD (Data)
import qualified Data.Typeable as DT (Typeable)

-- Posn (Row, Column)
newtype Position = Posn (Int, Int)
    deriving (Bounded, Eq, DD.Data, Ord, Read, DT.Typeable)

instance Show Position where
    show (Posn (row, col)) = show row ++ "," ++ show col

defaultPosn :: Position
defaultPosn = Posn (0,0)
