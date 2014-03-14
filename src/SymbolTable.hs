{- |
    Symbol table based on the LeBlanc-Cook symbol table abstraction
 -}
module SymbolTable
    ( SymTable
    , emptyTable
    , insert
    , lookup
    , update
    , accessible

    , SymInfo(..)
    , emptySymInfo

    , Scope(..)
    , ScopeNum

    , Value(..)

    , Stack
    , initialStack
    , pop
    , push
    ) where

import           Language      (Category (CatVariable), DataType (Void),
                                Identifier, Position)

import qualified Data.Map      as DM
import           Data.Maybe    (fromJust)
import           Data.Sequence as DS hiding (empty, update)
import           Prelude       hiding (lookup)

data SymInfo = SymInfo
    { dataType    :: DataType
    , category    :: Category
    , value       :: Maybe Value
    , scopeNum    :: ScopeNum
    , declPosn    :: Position
    , initialized :: Bool
    } deriving (Show)

emptySymInfo :: SymInfo
emptySymInfo = SymInfo {
                 dataType    = Void,
                 category    = CatVariable,
                 value       = Nothing,
                 scopeNum    = -1,
                 declPosn    = (0, 0), -- TODO Esto se debe conseguir del AlexPosn
                 initialized = False
               }

--------------------------------------------------------------------------------

data Scope = Scope { serial :: ScopeNum } deriving (Show)

initialScope :: Scope
initialScope = Scope { serial = 0 }

type ScopeNum = Int

--------------------------------------------------------------------------------

data Value
    = ValInt  Int
    | ValBool Bool
    | ValChar Char
    | ValFloat Float
    deriving (Eq)

instance Show Value where
    show (ValInt v)        = show v
    show (ValBool v)       = show v
    show (ValChar v)       = show v
    show (ValFloat v)      = show v

----------------------------------------

{- |
    Symbol Table
-}
data SymTable = SymTable (DM.Map Identifier (Seq SymInfo))
    deriving (Show)

{- |
    Empty symbol table
 -}
emptyTable :: SymTable
emptyTable = SymTable DM.empty

{- |
    Adds a symbol to the symbol table along with its information
 -}
insert :: Identifier -> SymInfo -> SymTable -> SymTable
insert vn info (SymTable m) = SymTable $ DM.alter f vn m
    where f Nothing  = Just $ DS.singleton info
          f (Just x) = Just $ info <| x

{- |
    Looks up the symbol identified by var in the symbol table
 -}
lookup :: Identifier -> SymTable -> Maybe SymInfo
lookup var (SymTable m) = do
    is <- DM.lookup var m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

{- |
    Update Actualiza el value de una variable, solo si la variable esta
    en la tabla de símbolos
 -}
update :: Identifier -> (SymInfo -> SymInfo) -> SymTable -> SymTable
update var f (SymTable m) = SymTable $ DM.alter func var m
    where
        func (Just is) = case viewl is of
            i :< iss  -> Just $ f i <| iss
            _         -> error "SymbolTable.update: No value to update"

{- |
    Returns all the variables -----  MAYBE ONLY VISIBLE???
 -}
accessible :: SymTable -> Seq (Identifier, SymInfo)
accessible st@(SymTable m) = fromList . map (\var -> (var, fromJust $ lookup var st)) $ DM.keys m

--------------------------------------------------------------------------------

newtype Stack a = Stack [a]
    deriving (Show)

push :: a -> Stack a -> Stack a
push element (Stack s) = Stack $ element : s

pop :: Stack a -> (a, Stack a)
pop (Stack [])      = error "SymbolTable.pop: Empty stack"
pop (Stack (x : s)) = (x, Stack s)

initialStack :: Stack Scope
initialStack = Stack [initialScope]
