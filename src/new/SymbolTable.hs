{-# LANGUAGE TupleSections #-}
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
    --, isProcedure

    , Initialized
    , Used
    , Pure
    , Offset
    , Bytes

    , SymbolCategory(..)
    , symbolCategory

    -- From Stack
    , Stack(..)
    , top
    , pop
    , push
    , initialStack
    , emptyStack
    --, singletonStack

    -- From Scope
    , Scope(..)
    , ScopeNum
    --, initialScope
    --, outerScope
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
            list' = concatMap (\(idn,ss) -> zip (repeat idn) ss) list
            list'' :: [(ScopeNum, [(Identifier, Symbol)])]
            list'' = map (\ls@((_,s):_) -> (scopeNum s,ls)) $ groupBy ((==) `on` (scopeNum . snd)) $ sortBy (compare `on` (scopeNum . snd)) list'
            list''' :: [(ScopeNum, [(Identifier, Symbol)])]
            list''' = map (second (sortBy (compare `on` (defPosn . snd)))) list''
            list'''' :: [String]
            list'''' = map (\(sc,infos) -> show sc ++ " -> " ++ concatMap (\(idn,si) -> "\n\t\t'" ++ idn ++ "':\t" ++ show si) infos) list'''

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
lookup idn (SymTable m) = do
    is <- DM.lookup idn m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

toListFilter :: ScopeNum -> SymbolTable -> Seq (Identifier, Symbol)
toListFilter sc st@(SymTable m) = fromList $ foldl' func [] $ DM.keys m
    where
        func ls idn = maybe ls (\j -> (idn, j) : ls) $ maySI idn
        maySI idn   = lookupWithScope idn (singletonStack (Scope sc)) st

{- |
    Looks up the symbol identifier in the specified scope in the symbol table
 -}
lookupWithScope :: Identifier -> Stack Scope -> SymbolTable -> Maybe Symbol
lookupWithScope idn (Stack scopes) (SymTable m) = do
    is <- DM.lookup idn m
    msum $ map ((\sc -> find ((sc==) . scopeNum) is) . serial) scopes

{- |
    Updates the Symbol of a given idntifier.
 -}
update :: Identifier -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
update idn f (SymTable m) = SymTable $ DM.alter func idn m
    where
        func mayIs = case mayIs of
            Just is -> case viewl is of
                i :< iss -> Just $ f i <| iss
                _        -> error $ "SymbolTable.update: No value to update for '" ++ idn ++ "'"
            Nothing -> error $ "SymbolTable.update: Identifier '" ++ idn ++ "' does not exist in symbol table"

{- |
    Updates the Symbol of a given identifier, in a given scope.
-}
updateWithScope :: Identifier -> ScopeNum -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
updateWithScope idn sc f (SymTable m) = SymTable $ DM.alter func idn m
    where
        func = maybe failure (Just . foldr foldFunc empty)
        failure = error $ "SymbolTable.update: Identifier '" ++ idn ++ "' does not exist in symbol table"
        foldFunc i is = if scopeNum i == sc
            then f i <| is
            else   i <| is

{- |
    Returns all the variables
 -}
accessible :: SymbolTable -> Seq (Identifier, Symbol)
accessible (SymTable m) = fromList . sortIt . expand $ DM.toList m
    where
        expand = concatMap (\(idn, syms) -> fmap (idn,) (toList syms))
        sortIt = sortBy (compare `on` (defPosn . snd))
--accessible (SymTable m) = fromList . sortBy (compare `on` (defPosn . snd)) $ DM.toList m
