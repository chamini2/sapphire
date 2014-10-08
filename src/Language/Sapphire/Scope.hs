module Language.Sapphire.Scope
    ( Scope(..)
    , ScopeNum

    , topScope
    , globalScope
    , langScope

    , topScopeNum
    , globalScopeNum
    , langScopeNum
    ) where

newtype Scope = Scope { serial :: ScopeNum }
    deriving (Eq)

instance Show Scope where
    show = show . serial

{- |
    The outermost scope in the program has a default value of 1
-}
topScope :: Scope
topScope = Scope { serial = topScopeNum }

{- |
    The outermost scope in the program has a default value of 0
-}
globalScope :: Scope
globalScope = Scope { serial = globalScopeNum }

{-
    The scope that contanis all the language definitions, (-1)
-}
langScope :: Scope
langScope = Scope { serial = langScopeNum }

----------------------------------------

type ScopeNum = Int

topScopeNum :: ScopeNum
topScopeNum = 1

globalScopeNum :: ScopeNum
globalScopeNum = 0

langScopeNum :: ScopeNum
langScopeNum = -1
