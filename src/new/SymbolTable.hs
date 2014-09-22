{-# LANGUAGE NamedFieldPuns, TupleSections  #-}
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
    , allSymbols

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

import           Program
import           Scope
import           Stack

import           Control.Arrow   (second)
import           Data.Foldable   as DF (concatMap, find, foldl', foldr, msum,
                                        toList)
import           Data.Function   (on)
import           Data.List       (groupBy, sortBy, intercalate)
import qualified Data.Map.Strict as Map (Map, alter, empty, keys, lookup,
                                         toList)
import           Data.Sequence   as DS (Seq, ViewL (..), empty, fromList,
                                        singleton, viewl, (<|))
import           Prelude         as P hiding (concatMap, foldr, lookup)

{- |
    Symbol Table
-}
newtype SymbolTable = SymTable { getMap :: Map.Map Identifier (Seq Symbol) }

showTable :: Int -> SymbolTable -> String
showTable t tab = tabs ++ "Symbol Table:\n" ++ concatMap (++ ("\n" ++ tabs)) showSymbols
        where
            allSyms :: [(Identifier, Symbol)]
            allSyms = toList $ allSymbols tab
            sortIt :: [(Identifier, Symbol)]
            sortIt = sortBy (compareOn scopeNum) allSyms
            groupIt :: [[(Identifier, Symbol)]]
            groupIt = groupBy (equalOn scopeNum) sortIt
            groupItKey :: [(ScopeNum, [(Identifier, Symbol)])]
            groupItKey = map (\ls@((_,s):_) -> (scopeNum s,ls)) groupIt
            sortInner :: [(ScopeNum, [(Identifier, Symbol)])]
            sortInner = map (second (sortBy (compareOn defPosn))) groupItKey
            showSymbols :: [String]
            showSymbols = map (uncurry showScpInfs) sortInner
            ----------------------------------------
            tabs :: String
            tabs = replicate t '\t'
            equalOn :: Eq a => (Symbol -> a) -> (Identifier, Symbol) -> (Identifier, Symbol) -> Bool
            equalOn f = (==) `on` (f . snd)
            compareOn :: Ord a => (Symbol -> a) -> (Identifier, Symbol) -> (Identifier, Symbol) -> Ordering
            compareOn f = compare `on` (f . snd)
            showScpInfs :: ScopeNum -> [(Identifier, Symbol)] -> String
            showScpInfs scp infs = tabs ++ "\t" ++ show scp ++ " -> " ++ concatMap (uncurry showInf) infs
            showInf :: Identifier -> Symbol -> String
            showInf idn sym = "\n\t\t" ++ tabs ++ "'" ++ idn ++ "':\t" ++ show sym


instance Show SymbolTable where
    show = showTable 0

--------------------------------------------------------------------------------

type Initialized     = Bool
type Used            = Bool
type Pure            = Bool
type LanguageDefined = Bool
type Offset          = Int
type Bytes           = Int

data Symbol = SymInfo
                { dataType   :: Lexeme DataType
                , category   :: Category
                , offset     :: Offset
                , bytes      :: Bytes
                , used       :: Used
                , scopeNum   :: ScopeNum
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            | SymType
                { dataType   :: Lexeme DataType
                , fields     :: Maybe SymbolTable
                , langDef    :: LanguageDefined
                , bytes      :: Bytes
                , used       :: Used
                , scopeNum   :: ScopeNum
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            | SymFunction
                { paramTypes :: Seq (Lexeme DataType)
                , returnType :: Lexeme DataType
                , body       :: StBlock
                , returned   :: Bool
                , langDef    :: LanguageDefined
                , bytes      :: Bytes
                , used       :: Used
                , scopeNum   :: ScopeNum
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }

instance Show Symbol where
    show sym = case sym of
        SymInfo dt cat off by u scp stk p -> intercalate ", " [showScp scp, showP p, showCat, showDT, showU u, showBy by, showOff, showStk stk]
            where
                showCat = show cat
                showDT  = show $ lexInfo dt
                showOff = "offset is " ++ show off
        SymType dt flds lDef by u scp stk p -> intercalate ", " [showScp scp, showP p, showLDef, showDT, showU u, showBy by, showStk stk, showFlds]
            where
                showLDef = if lDef then "language-defined" else "user-defined"
                showDT    = show $ lexInfo dt
                showFlds  = maybe "NO Symbol Table" ((++) "\n" . showTable 3) flds
        SymFunction prms rt _ _ lDef by u scp stk p -> intercalate ", " [showScp scp, showP p, showLDef, showSign, showU u, showBy by, showStk stk]
            where
                showLDef = if lDef then "language-defined" else "user-defined"
                showSign = "(" ++ intercalate "," (map (show . lexInfo) $ toList prms) ++ ") -> " ++ show (lexInfo rt)
        where
            showScp scp = "scope " ++ show scp
            showP p     = "(" ++ show p ++ ")"
            showU u     = if u then "used" else "NOT used"
            showBy by   = show by ++ " bytes"
            showStk stk = "stack: " ++ show stk

--instance Show Symbol where
--    show (SymInfo dt ct v sn dp i u p o) = showSN ++ showCT ++ showV ++ showDT ++ showDP ++ showU ++ showP ++ showO
--        where
--            showSN = "Scope " ++ show sn ++ ",\t"
--            showCT = show ct ++ " | "
--            showDT = show dt ++ case dt of
--                Record _ _ w -> " [" ++ show w ++ "] "
--                Union  _ _ w -> " [" ++ show w ++ "] "
--                Array  _ _ w -> " [" ++ show w ++ "] "
--                _            -> " "
--            showV  = showI ++ maybe "" show v ++ "\t"
--                where
--                    showI  = "(" ++ (if i then "init" else "NOT init") ++ ")"
--            showDP = show dp
--            showU  = " (" ++ (if u then "used" else "NOT used") ++ ")"
--            showP  = " (" ++ (if p then "pure" else "impure") ++ ")"
--            showO  = " (offset " ++ show o ++ ")"

----------------------------------------

data SymbolCategory = CatInfo
                    | CatType
                    | CatFunction
                    deriving (Eq, Show)

--------------------------------------------------------------------------------

{- |
    Empty symbol table
 -}
emptyTable :: SymbolTable
emptyTable = SymTable Map.empty

----------------------------------------

emptySymInfo :: Symbol
emptySymInfo = SymInfo
    { dataType   = fillLex Void
    , category   = CatVariable
    , offset     = 0
    , bytes      = 0
    , used       = False
    , scopeNum   = -1
    , scopeStack = singletonStack outerScope
    , defPosn    = defaultPosn
    }

emptySymType :: Symbol
emptySymType = SymType
    { dataType   = fillLex Void
    , fields     = Nothing
    , langDef    = False
    , bytes      = 0
    , used       = False
    , scopeNum   = -1
    , scopeStack = singletonStack outerScope
    , defPosn    = defaultPosn
    }

emptySymFunction :: Symbol
emptySymFunction = SymFunction
    { paramTypes = empty
    , returnType = fillLex Void
    , returned   = False
    , body       = empty
    , langDef    = False
    , bytes      = 0
    , used       = False
    , scopeNum   = -1
    , scopeStack = singletonStack outerScope
    , defPosn    = defaultPosn
    }

----------------------------------------

isProcedure :: Symbol -> Bool
isProcedure sym = case sym of
    SymFunction { returnType } -> lexInfo returnType == Void
    _ -> error "Symbol.isProcedure: asking if non-function symbol is procedure"

----------------------------------------

symbolCategory :: Symbol -> SymbolCategory
symbolCategory sym = case sym of
    SymInfo {}     -> CatInfo
    SymType {}     -> CatType
    SymFunction {} -> CatFunction

--------------------------------------------------------------------------------

{- |
    Adds a symbol to the symbol table along with its information
 -}
insert :: Identifier -> Symbol -> SymbolTable -> SymbolTable
insert vn info = SymTable . Map.alter func vn . getMap
    where
        func Nothing  = Just $ singleton info
        func (Just x) = Just $ info <| x

----------------------------------------

{- |
    Looks up the symbol identified by var in the symbol table
 -}
lookup :: Identifier -> SymbolTable -> Maybe Symbol
lookup idn (SymTable m) = do
    is <- Map.lookup idn m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

{- |
    Looks up the symbol identifier in the specified scope in the symbol table
 -}
lookupWithScope :: Identifier -> Stack Scope -> SymbolTable -> Maybe Symbol
lookupWithScope idn (Stack scopes) (SymTable m) = do
    is <- Map.lookup idn m
    msum $ map ((\sc -> find ((sc==) . scopeNum) is) . serial) scopes

----------------------------------------

{- |
    Updates the Symbol of a given idntifier.
 -}
update :: Identifier -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
update idn f = SymTable . Map.alter func idn . getMap
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
updateWithScope idn sc f = SymTable . Map.alter func idn . getMap
    where
        func = maybe failure (Just . foldr foldFunc empty)
        failure = error $ "SymbolTable.update: Identifier '" ++ idn ++ "' does not exist in symbol table"
        foldFunc i is = if scopeNum i == sc
            then f i <| is
            else   i <| is

----------------------------------------

toListFilter :: ScopeNum -> SymbolTable -> Seq (Identifier, Symbol)
toListFilter sc st@(SymTable m) = fromList $ foldl' func [] $ Map.keys m
    where
        func ls idn = maybe ls (\j -> (idn, j) : ls) $ maySI idn
        maySI idn   = lookupWithScope idn (singletonStack (Scope sc)) st

{- |
    Returns all the Symbols
 -}
allSymbols :: SymbolTable -> Seq (Identifier, Symbol)
allSymbols = fromList . sortIt . expand . Map.toList . getMap
    where
        expand = concatMap (\(idn, syms) -> fmap (idn,) (toList syms))
        sortIt = sortBy (compare `on` (defPosn . snd))
--accessible (SymTable m) = fromList . sortBy (compare `on` (defPosn . snd)) $ Map.toList m
