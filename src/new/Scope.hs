module Scope
    ( Scope(..)
    , ScopeNum

    , initialScope
    ) where

data Scope = Scope { serial :: ScopeNum } deriving (Show)

{- |
    The outermost scope has a default value of 0
-}
initialScope :: Scope
initialScope = Scope { serial = 0 }

type ScopeNum = Int
