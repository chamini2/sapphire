module Language.Sapphire.Scope
    ( Scope(..)
    , ScopeNum

    , topScopeNum
    , langScopeNum
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
topScope = Scope { serial = topScopeNum }

{-
    The scope that contanis all the language definitions
-}
langScope :: Scope
langScope = Scope { serial = langScopeNum }

----------------------------------------

type ScopeNum = Int

topScopeNum :: ScopeNum
topScopeNum = 0

langScopeNum :: ScopeNum
langScopeNum = -1
