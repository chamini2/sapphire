module Stack
    ( Stack(..)
    , top
    , pop
    , push

    , initialStack
    , emptyStack
    , singletonStack
    ) where

import           Scope

import           Data.Foldable as DF (Foldable (..), concatMap)
import           Prelude       as P hiding (concatMap)

newtype Stack a = Stack [a]

instance Show a => Show (Stack a) where
    show (Stack s) = drop 1 $ concatMap ((++) "\n\t" . show) s

instance Functor Stack where
    fmap f (Stack s) = Stack $ map f s

instance Foldable Stack where
    foldr f b (Stack s) = P.foldr f b s

{- |
    Shows the first element in the stack, if there are any, without popping it.
-}
top :: Stack a -> a
top (Stack [])      = error "SymbolTable.top: Empty stack"
top (Stack (x : _)) = x

{- |
    Pushes an element to the stack.
-}
push :: a -> Stack a -> Stack a
push element (Stack s) = Stack $ element : s

{- |
    Pops an element from the stack.
-}
pop :: Stack a -> Stack a
pop (Stack [])      = error "SymbolTable.pop: Empty stack"
pop (Stack (_ : s)) = Stack s

{- |
    The scope stack has the inital scope by default.
-}
initialStack :: Stack Scope
initialStack = Stack [ initialScope, outerScope ]

emptyStack :: Stack a
emptyStack = Stack [ ]

singletonStack :: a -> Stack a
singletonStack n = Stack [ n ]
