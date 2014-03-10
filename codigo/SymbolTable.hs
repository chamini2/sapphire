{-|
    Symbol table based on the LeBlanc-Cook symbol table abstraction 
 -}
module SymbolTable
    -- SymTable
    ( SymTable
    , emptyTable
    , insert
    , lookup
    , update
    -- SymInfo
    , SymInfo(..)
    -- Scope
    , Scope(..)
    , ScopeNum
    -- Value according to the Sapphire language definition
    , Value(..)
    -- Stack
    , Stack
    , emptyStack
    , pop
    , push
    ) where

import           Language      (DataType, Identifier)

import           Prelude       hiding (lookup)
import qualified Data.Map      as DM
import           Data.Sequence as DS hiding (empty, update)


data SymInfo = SymInfo
    { dataType :: DataType
    , value    :: Maybe Value
    , scopeNum :: ScopeNum
    , position :: Position
    } deriving (Show)

type Position = (Int, Int) -- (Fila, Columna)

data Scope = Scope { serial :: ScopeNum } deriving (Show)

initialScope = Scope { serial = 0 }

type ScopeNum = Int

newtype Stack a = Stack [a]
    deriving (Show)

push :: a -> Stack a -> Stack a
push element (Stack s) = Stack $ element : s

pop :: Stack a -> (a, Stack a)
pop (Stack [])      = error "empty stack"
pop (Stack (x : s)) = (x, Stack s)

emptyStack :: Stack Scope
emptyStack = Stack [initialScope]

data Value
    = Int  Int
    | Bool Bool
    | Char Char
    | Float Float
    | Void
    deriving (Eq)

instance Show Value where
    show (Int v)   = show v
    show (Bool v)  = show v
    show (Char v)  = show v
    show (Float v) = show v
    show (Void)    = "void"

{-|
    Symbol Table
-}
data SymTable = SymTable (DM.Map Identifier (Seq SymInfo))
    deriving (Show)

{-|
    Empty symbol table
 -}
emptyTable :: SymTable
emptyTable = SymTable DM.empty

{-|
    Adds a symbol to the symbol table along with its information
 -}
insert :: Identifier -> SymInfo -> SymTable -> SymTable
insert vn info (SymTable m) = SymTable $ DM.alter f vn m
    where f Nothing  = Just $ DS.singleton info
          f (Just x) = Just $ info <| x

{-|
    Looks up the symbol identified by id in the symbol table
 -}
lookup :: Identifier -> SymTable -> Maybe SymInfo
lookup id (SymTable m) = do
    is <- DM.lookup id m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

{-|
    Update Actualiza el value de una variable, solo si la variable esta
    en la tabla de símbolos
 -}
update :: Identifier -> Value -> SymTable -> SymTable
update id vn (SymTable m) = SymTable $ DM.alter f id m
    where f (Just is) =
            case viewl is of
                i :< iss -> Just $ i { value = Just vn } <| iss
