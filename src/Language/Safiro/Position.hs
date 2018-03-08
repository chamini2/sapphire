module Language.Safiro.Position
    ( Position(..)
    , row
    , col
    , defaultPosn
    ) where

-- Posn (Row, Column)
newtype Position = Posn (Int, Int)
    deriving (Bounded, Eq)

instance Ord Position where
    compare (Posn (r1, c1)) (Posn (r2, c2)) =
        case compare r1 r2 of
            EQ    -> compare c1 c2
            other -> other

instance Show Position where
    show (Posn tuple) = case tuple of
        (0,0) -> "in the language"
        -- When we only have the line number
        (r,0) -> "at " ++ show r
        (r,c) -> "at " ++ show r ++ ":" ++ show c

defaultPosn :: Position
defaultPosn = Posn (0,0)

row :: Position -> Int
row (Posn (r,_)) = r

col :: Position -> Int
col (Posn (_,c)) = c
