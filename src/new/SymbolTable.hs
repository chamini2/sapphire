{- |
    Symbol table based on the LeBlanc-Cook symbol table abstraction
 -}
module SymbolTable
    ( SymbolTable
    , emptyTable
    , insert
    , lookup
    , lookupWithScope
    , toListFilter
    , update
    , updateWithScope
    , accessible

    -- From Symbol
    , Symbol(..)
    , emptySymInfo
    , emptySymType
    , emptySymFunction

    , Initialized
    , Used
    , Pure
    , Offset
    , Width

    , SymbolCategory(..)
    , symbolCategory

    -- From Stack
    , Stack(..)
    , top
    , pop
    , push
    , initialStack
    --, singletonStack

    -- From Scope
    , Scope(..)
    , ScopeNum
    --, initialScope
    ) where

import           Identifier
import           Scope
import           Stack
import           Symbol

import           Control.Arrow (second)
import           Data.Foldable as DF (concatMap, find, foldl', foldr, msum,
                                      toList)
import           Data.Function (on)
import           Data.List     (groupBy, sortBy)
import qualified Data.Map      as DM (Map, alter, empty, keys, lookup, toList)
import           Data.Sequence as DS (Seq, ViewL (..), empty, fromList,
                                      singleton, viewl, (<|))
import           Prelude       as P hiding (concatMap, foldr, lookup)

{- |
    Symbol Table
-}
newtype SymbolTable = SymTable (DM.Map Identifier (Seq Symbol))

instance Show SymbolTable where
    show (SymTable m) = "Symbol Table:\n" ++ concatMap (++"\n") list''''
        where
            list :: [(Identifier, [Symbol])]
            list = map (second toList) $ DM.toList m
            list' :: [(Identifier, Symbol)]
            list' = concatMap (\(var,ss) -> zip (repeat var) ss) list
            list'' :: [(ScopeNum, [(Identifier, Symbol)])]
            list'' = map (\ls@((_,s):_) -> (scopeNum s,ls)) $ groupBy ((==) `on` (scopeNum . snd)) $ sortBy (compare `on` (scopeNum . snd)) list'
            list''' :: [(ScopeNum, [(Identifier, Symbol)])]
            list''' = map (second (sortBy (compare `on` (defPosn . snd)))) list''
            list'''' :: [String]
            list'''' = map (\(sc,infos) -> show sc ++ " -> " ++ concatMap (\(iden,si) -> "\n\t\t" ++ iden ++ ":\t" ++ show si) infos) list'''

--------------------------------------------------------------------------------

{- |
    Empty symbol table
 -}
emptyTable :: SymbolTable
emptyTable = SymTable DM.empty

----------------------------------------

{- |
    Adds a symbol to the symbol table along with its information
 -}
insert :: Identifier -> Symbol -> SymbolTable -> SymbolTable
insert vn info (SymTable m) = SymTable $ DM.alter func vn m
    where
        func Nothing  = Just $ singleton info
        func (Just x) = Just $ info <| x

----------------------------------------

{- |
    Looks up the symbol identified by var in the symbol table
 -}
lookup :: Identifier -> SymbolTable -> Maybe Symbol
lookup var (SymTable m) = do
    is <- DM.lookup var m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

toListFilter :: SymbolTable -> ScopeNum -> Seq (Identifier, Symbol)
toListFilter st@(SymTable m) sc = fromList $ foldl' func [] $ DM.keys m
    where
        func ls iden = maybe ls (\j -> (iden, j) : ls) $ maySI iden
        maySI iden   = lookupWithScope iden (singletonStack (Scope sc)) st

{- |
    Looks up the symbol identifier in the specified scope in the symbol table
 -}
lookupWithScope :: Identifier -> Stack Scope -> SymbolTable -> Maybe Symbol
lookupWithScope var (Stack scopes) (SymTable m) = do
    is <- DM.lookup var m
    msum $ map ((\sc -> find ((sc==) . scopeNum) is) . serial) scopes

{- |
    Updates the Symbol of a given identifier.
 -}
update :: Identifier -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
update var f (SymTable m) = SymTable $ DM.alter func var m
    where
        func mayIs = case mayIs of
            Just is -> case viewl is of
                i :< iss -> Just $ f i <| iss
                _        -> error $ "SymbolTable.update: No value to update for '" ++ var ++ "'"
            Nothing -> error $ "SymbolTable.update: Identifier '" ++ var ++ "' does not exist in symbol table"

{- |
    Updates the Symbol of a given identifier, in a given scope.
-}
updateWithScope :: Identifier -> ScopeNum -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
updateWithScope var sc f (SymTable m) = SymTable $ DM.alter func var m
    where
        func = maybe failure (Just . foldr foldFunc empty)
        failure = error $ "SymbolTable.update: Identifier '" ++ var ++ "' does not exist in symbol table"
        foldFunc i is = if scopeNum i == sc
            then f i <| is
            else   i <| is

{- |
    Returns all the variables
 -}
accessible :: SymbolTable -> Seq (Identifier, Seq Symbol)
accessible (SymTable m) = fromList $ DM.toList m
