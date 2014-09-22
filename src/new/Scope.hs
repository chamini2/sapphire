module Scope
    ( Scope(..)
    , ScopeNum

    , initialScope
    , outerScope
    ) where

newtype Scope = Scope { serial :: ScopeNum }

instance Show Scope where
    show = show . serial

{- |
    The outermost scope in the program has a default value of 0
-}
initialScope :: Scope
initialScope = Scope { serial = 0 }

{-
    The scope that contanis all the language definitions
-}
outerScope :: Scope
outerScope = Scope { serial = -1 }

----------------------------------------

type ScopeNum = Int
