{-# LANGUAGE DeriveDataTypeable #-}
module Position
    ( Position(..)
    , row
    , col
    , defaultPosn
    ) where

import qualified Data.Data     as DD (Data)
import qualified Data.Typeable as DT (Typeable)

-- Posn (Row, Column)
newtype Position = Posn (Int, Int)
    deriving (Bounded, Eq, DD.Data, Read, DT.Typeable)

instance Ord Position where
    compare (Posn (r1, c1)) (Posn (r2, c2)) =
        if r1 /= r2
            then compare r1 r2
            else compare c1 c2

instance Show Position where
    show (Posn tuple) = case tuple of
        (0,0) -> "in the language"
        (r,c) -> "at " ++ show r ++ "," ++ show c

defaultPosn :: Position
defaultPosn = Posn (0,0)

row :: Position -> Int
row (Posn (r,_)) = r

col :: Position -> Int
col (Posn (_,c)) = c
