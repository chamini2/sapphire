{-|
    Tabla de símbolos basada en una tabla de hash con Strings como
    claves y un Data.Sequence de SymInfo como valuees
 -}
module SymbolTable
    ( SymTable
    , SymInfo(..)
    , Scope
    , Value(..)
    , empty
    , insert
    , lookup
    , update
    ) where

import           Language      (DataType, Identifier)

import           Prelude       hiding (lookup)
import qualified Data.Map      as DM
import           Data.Sequence as DS hiding (empty, update)

data SymTable = SymTable (DM.Map Identifier (Seq SymInfo))
    deriving (Show)

data SymInfo = SymInfo
    { sType  :: DataType
    , sValue :: Value
    , scope  :: Scope () -- `()` está mientras tanto
    , line   :: Int
    , column :: Int
    } deriving (Show)

newtype Scope a = Scope [a]
    deriving (Show)

push :: a -> Scope a -> Scope a
push element (Scope s) = Scope $ element : s

pop :: Scope a -> (a, Scope a)
pop (Scope [])      = error "empty stack"
pop (Scope (x : s)) = (x, Scope s)

data Value
    = Int  Int
    | Bool Bool
    | Char Char
    | Null
    deriving (Eq)

instance Show Value where
    show (Int v)  = show v
    show (Bool v) = show v
    show (Char v) = show v
    show Null     = "Null"

{-|
    Crear una tabla de símbolos vacia
 -}
empty :: SymTable
empty = SymTable DM.empty

{-|
    Agregar la variable vn a la tabla de símbolos
 -}
insert :: Identifier -> SymInfo -> SymTable -> SymTable
insert vn info (SymTable m) = SymTable $ DM.alter f vn m
    where f Nothing  = Just $ DS.singleton info
          f (Just x) = Just $ info <| x

{-|
    Buscar la información relacionada con una variable en la tabla de símbolos
 -}
lookup :: Identifier -> SymTable -> Maybe SymInfo
lookup name (SymTable m) = do
    is <- DM.lookup name m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

{-|
    Actualiza el value de una variable, solo si la variable esta
    en la tabla de símbolos
 -}
update :: Identifier -> Value -> SymTable -> SymTable
update name vn (SymTable m) = SymTable $ DM.alter f name m
    where f (Just is) =
            case viewl is of
                i :< iss -> Just $ i { sValue = vn } <| iss
