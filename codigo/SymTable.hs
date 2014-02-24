{-|
    Tabla de simbolos basada en una tabla de hash con Strings como 
    claves y un Data.Sequence de SymInfo como valuees         
-}
module SymTable (
    -- * Types exportados
    SymTable,
    SymInfo  (..),
    Stack,
    Scope,
    Value (..),
    -- * Funciones exportadas
    {-empty,-}
    insert,
    {-lookup,-}
    {-update,-}
    {-eliminarVariable,-}
)
where

import Data.Sequence as DS
import qualified Data.Map as DM

{-import Parser (Id, Type)-}
type Id = String

type Category = Variable
              | Constant
              | Function
              | Parameter
              | FieldName
              deriving (Show, Eq)

data SymTable = SymTable (DM.Map Id (Seq SymInfo))
    deriving (Show)

data SymInfo = SymInfo {
    s_type  :: DataType,
    {-s_cat   :: Category,-}
    s_value :: Value,
    scope   :: Stack,
    line    :: Int,
    column  :: Int
} deriving (Show)

data Scope = Scope {
    serial :: Int,
    active :: Bool
    {-extra  :: undefined-}
} deriving (Show)

newtype Stack a = Stack [a]
    deriving (Show)

push :: a -> Stack a -> Stack a
push elem (Stack s) = Stack $ elem : s

pop :: Stack a -> (a, Stack a)
pop (Stack [])      = error "empty stack"
pop (Stack (x : s)) = (x, Stack s)

data Value = Int Int 
           | Bool Bool
           | Char Char
           | Null
           deriving (Eq)

instance Show Value where 
    show (Int v) = show v
    show (Bool v) = show v
    show (Char v) = show v
    show Null     = "Null"

{-| 
    Crear una tabla de simbolos vacia    
-}
empty :: SymTable 
empty = SymTable (DM.empty)



{-|
    Agregar la variable v a la tabla de simbolos
-}
insert :: Id -> SymInfo -> SymTable -> SymTable 
insert vn info (SymTable m) = SymTable $ DM.alter f vn m
    where f Nothing  = Just $ DS.singleton info
          f (Just x) = Just $ info <| x

{-|
    Buscar la informacion relacionada con una variable en la tabla de simbolos
-}
lookup :: Id -> SymTable -> Maybe SymInfo 
lookup id (SymTable m) = do
    is <- DM.lookup id m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

{-|
    update el value de una variable, solo si la variable esta 
    en la tabla de simbolos
 -}
update :: Id -> Value -> SymTable -> SymTable 
update id vn (SymTable m) = SymTable $ DM.alter f id m
    where f (Just is) =
            case viewl is of 
                i :< iss -> Just $ i { s_value = vn } <| iss
