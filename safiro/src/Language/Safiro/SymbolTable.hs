{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{- |
    Symbol table based on the LeBlanc-Cook symbol table definition
 -}
module Language.Safiro.SymbolTable
    ( SymbolTable
    , emptyTable
    , member
    , insert
    , lookup
    , lookupWithScope
    , update
    , updateWithScope
    , toSeq
    , fromSeq

    , Symbol(..)
    , scope
    , emptySymInfo
    , emptySymType
    , emptySymFunction

    , Used
    , LanguageDefined
    , Returned
    , Offset
    , Width

    , SymbolCategory(..)
    , symbolCategory

    , module Language.Safiro.Stack
    , module Language.Safiro.Scope
    ) where

import           Language.Safiro.Program
import           Language.Safiro.Scope
import           Language.Safiro.Stack

import           Data.Foldable             (concatMap, find, foldr, msum,
                                            toList)
import           Data.Function             (on)
import           Data.List                 (groupBy, intercalate, sortBy)
import qualified Data.Map.Strict           as Map (Map, adjust, elems, empty,
                                                   insert, lookup, member,
                                                   singleton, toList)
import           Data.Maybe                (isJust)
import           Data.Sequence             (Seq, empty, fromList)
import           Prelude                   hiding (concatMap, foldr, lookup)
import qualified Prelude                   as P (fmap)

type SymbolTable = Map.Map Identifier (Map.Map Scope Symbol)

instance Show SymbolTable where
    show = showTable 0

showTable :: Int -> SymbolTable -> String
showTable t tab = tabs ++ "Symbol Table:\n" ++ concatMap (++ ("\n" ++ tabs)) showSymbols
    where
        allSyms :: [(Identifier, Symbol)]
        allSyms = toList $ toSeq tab
        sortIt :: [(Identifier, Symbol)]
        sortIt = sortBy (compareOn scope) allSyms
        groupIt :: [[(Identifier, Symbol)]]
        groupIt = groupBy (equalOn scope) sortIt
        groupItKey :: [(Scope, [(Identifier, Symbol)])]
        groupItKey = map (\ls@((_,s):_) -> (scope s,ls)) groupIt
        showSymbols :: [String]
        showSymbols = map (uncurry showScpInfs) groupItKey
        ----------------------------------------
        tabs :: String
        tabs = replicate t '\t'
        equalOn :: Eq a => (Symbol -> a) -> (Identifier, Symbol) -> (Identifier, Symbol) -> Bool
        equalOn f = (==) `on` (f . snd)
        compareOn :: Ord a => (Symbol -> a) -> (Identifier, Symbol) -> (Identifier, Symbol) -> Ordering
        compareOn f = compare `on` (f . snd)
        showScpInfs :: Scope -> [(Identifier, Symbol)] -> String
        showScpInfs scp infs = tabs ++ "\t" ++ show scp ++ " -> " ++ concatMap (uncurry showInf) infs
        showInf :: Identifier -> Symbol -> String
        showInf idn sym = "\n\t\t" ++ tabs ++ "'" ++ idn ++ "':" ++ replicate (3 - div (length idn + 3) 4) '\t' ++ show sym

--------------------------------------------------------------------------------

type Used            = Bool
type LanguageDefined = Bool
type Returned        = Bool
type Offset          = Int
type Width           = Int

data Symbol = SymInfo
                { dataType   :: Lexeme DataType
                , category   :: Category
                , offset     :: Offset
                , width      :: Width
                , used       :: Used
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            | SymType
                { dataType   :: Lexeme DataType
                , fields     :: Maybe SymbolTable
                , langDef    :: LanguageDefined
                , width      :: Width
                , used       :: Used
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            | SymFunction
                { paramTypes :: Seq (Lexeme DataType)
                , returnType :: Lexeme DataType
                , body       :: StBlock
                , returned   :: Returned
                , langDef    :: LanguageDefined
                , blockWidth :: Width
                , prmsWidth  :: Width
                , used       :: Used
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }

instance Show Symbol where
    show sym = case sym of
        SymInfo dt cat off by u stk p           -> intercalate ", " [showP p, showCat, showDT, showU u, showW by, showOff, showStk stk]
            where
                showCat = show cat
                showDT  = show $ lexInfo dt
                showOff = "offset is " ++ show off
        SymType dt flds lDef by u stk p         -> intercalate ", " [showP p, showCat, showLDef, showDT, showU u, showW by, showStk stk, showFlds]
            where
                showCat  = show CatType
                showLDef = if lDef then "language-defined" else "user-defined"
                showDT   = show $ lexInfo dt
                showFlds = maybe "NO Symbol Table" ((++) "\n" . showTable 3) flds
        SymFunction prms rt _ _ lDef bBy pBy u stk p -> intercalate ", " [showP p, showCat, showLDef, showSign, showU u, showW bBy, showW pBy, showStk stk]
            where
                showCat  = show CatFunction
                showLDef = if lDef then "language-defined" else "user-defined"
                showSign = "(" ++ intercalate "," (map (show . lexInfo) $ toList prms) ++ ") -> " ++ show (lexInfo rt)
        where
            showP p  = "(" ++ show p ++ ")"
            showU u  = if u then "used" else "NOT used"
            showW by = show by ++ " bytes"
            showStk  = ("stack: " ++) . show

----------------------------------------

scope :: Symbol -> Scope
scope = top . scopeStack

----------------------------------------

data SymbolCategory = CatInfo
                    | CatType
                    | CatFunction
                    deriving (Eq)

instance Show SymbolCategory where
    show = \case
        CatInfo     -> "variable"
        CatType     -> "data type"
        CatFunction -> "function"

-- To sort types first, variables second, and functions last
instance Ord SymbolCategory where
    compare x y = case (x, y) of
        (CatInfo    , CatInfo    ) -> EQ
        (CatType    , CatType    ) -> EQ
        (CatFunction, CatFunction) -> EQ
        (CatType    , _          ) -> LT
        (_          , CatFunction) -> LT
        (_          , CatType    ) -> GT
        (CatFunction, _          ) -> GT

--------------------------------------------------------------------------------

{- |
    Empty symbol table
 -}
emptyTable :: SymbolTable
emptyTable = Map.empty

----------------------------------------

emptySymInfo :: Symbol
emptySymInfo = SymInfo
    { dataType   = pure Void
    , category   = CatVariable
    , offset     = 0
    , width      = 0
    , used       = False
    , scopeStack = langStack
    , defPosn    = defaultPosn
    }

emptySymType :: Symbol
emptySymType = SymType
    { dataType   = pure Void
    , fields     = Nothing
    , langDef    = False
    , width      = 0
    , used       = False
    , scopeStack = langStack
    , defPosn    = defaultPosn
    }

emptySymFunction :: Symbol
emptySymFunction = SymFunction
    { paramTypes = empty
    , returnType = pure Void
    , returned   = False
    , body       = empty
    , langDef    = False
    , blockWidth = 0
    , prmsWidth  = 0
    , used       = False
    , scopeStack = langStack
    , defPosn    = defaultPosn
    }

----------------------------------------

symbolCategory :: Symbol -> SymbolCategory
symbolCategory sym = case sym of
    SymInfo {}     -> CatInfo
    SymType {}     -> CatType
    SymFunction {} -> CatFunction

--------------------------------------------------------------------------------

member :: Identifier -> Stack Scope -> SymbolTable -> Bool
member idn scps = isJust . lookupWithScope idn scps

----------------------------------------

{- |
    Adds a symbol to the symbol table along with its information
 -}
insert :: Identifier -> Symbol -> SymbolTable -> SymbolTable
insert idn sym tab = if Map.member idn tab
    then Map.adjust inner idn tab
    else Map.insert idn (Map.singleton scp sym) tab
    where
        inner scpTab = if Map.member scp scpTab
            then error "SymbolTable.insert: inserting Symbol already in SymbolTable"
            else Map.insert scp sym scpTab
        scp = scope sym

----------------------------------------

{- |
    Looks up the symbol identified by var in the symbol table
 -}
lookup :: Identifier -> SymbolTable -> Maybe Symbol
lookup idn tab = Map.lookup idn tab >>= return . head . Map.elems

{- |
    Looks up the symbol identifier in the specified scope in the symbol table
 -}
lookupWithScope :: Identifier -> Stack Scope -> SymbolTable -> Maybe Symbol
lookupWithScope idn scps tab = do
    scpTab <- Map.lookup idn tab
    msum $ P.fmap (flip Map.lookup scpTab) scps

----------------------------------------

{- |
    Updates the Symbols of a given idntifier.
 -}
update :: Identifier -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
update idn f tab = if Map.member idn tab
    then Map.adjust (P.fmap f) idn tab
    else error "SymbolTable.update: updating symbol that does not exist in SymbolTable"

{- |
    Updates the Symbol of a given identifier, in a given scope.
-}
updateWithScope :: Identifier -> Stack Scope -> (Symbol -> Symbol) -> SymbolTable -> SymbolTable
updateWithScope idn scps f tab = Map.adjust func idn tab
    where
        func scpTab = maybe tellError updateSym $ find condition scps
            where
                updateSym scp = Map.adjust f scp scpTab
                condition scp = Map.member scp scpTab
                tellError = error "SymbolTable.updateWithScope: updating symbol that does not exist in SymbolTable"

--------------------------------------------------------------------------------

{- |
    Returns all the Symbols
 -}
toSeq :: SymbolTable -> Seq (Identifier, Symbol)
toSeq = fromList . sortIt . expand . Map.toList
    where
        expand   = concatMap (\(idn, syms) -> zip (repeat idn) (toList syms))
        sortIt   = sortBy comp
        comp x y = case compOn symbolCategory x y of
            EQ    -> compOn defPosn x y
            other -> other
        compOn f = compare `on` (f . snd)

{- |
    Creates a SymbolTable from a Seq
 -}
fromSeq :: Seq (Identifier, Symbol) -> SymbolTable
fromSeq = foldr (uncurry insert) emptyTable
