module Scope
    ( Scope(..)
    , ScopeNum

    , topScope
    , langScope
    ) where

newtype Scope = Scope { serial :: ScopeNum }
    deriving (Eq)

instance Show Scope where
    show = show . serial

{- |
    The outermost scope in the program has a default value of 0
-}
topScope :: Scope
topScope = Scope { serial = 0 }

{-
    The scope that contanis all the language definitions
-}
langScope :: Scope
langScope = Scope { serial = -1 }

----------------------------------------

type ScopeNum = Int
