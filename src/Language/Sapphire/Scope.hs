module Language.Sapphire.Scope
    ( Scope

    , topScope
    , globalScope
    , langScope
    ) where

type Scope = Int

topScope :: Scope
topScope = 1

globalScope :: Scope
globalScope = 0

langScope :: Scope
langScope = -1
