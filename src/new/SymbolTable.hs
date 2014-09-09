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

    , SymbolInfo(..)
    , emptySymbolInfo

    , Initialized
    , Used
    , Pure
    , Offset
    , Width

    , Value(..)

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

import           Language
import           Scope
import           Stack

import           Data.Foldable as DF
import           Control.Arrow (second)
import           Data.Function (on)
import           Data.List     (groupBy, sortBy)
import qualified Data.Map      as DM
import           Data.Sequence as DS hiding (zip, drop, update, sortBy)
import           Prelude       as P hiding (concatMap, lookup, concat)

{-data SymbolInfo = SymInfo
                    { dataType :: DataType
                    , category :: Category
                    , scopeNum :: ScopeNum
                    , defPosn  :: Position
                    , offset   :: Offset
                    , width    :: Width
                    , used     :: Used
                    }
                | SymType
                    { dataType :: DataType
                    , width    :: Width
                    , used     :: Used
                    }
                | SymFunction
                    { parameters :: Seq (Lexeme DataType)
                    , body       :: StBlock
                    , width      :: Width
                    , used       :: Used
                    }
                deriving Show
-}
data SymbolInfo = SymInfo
    { dataType :: DataType
    , category :: Category
    , value    :: Maybe Value
    , scopeNum :: ScopeNum
    , defPosn  :: Position
    , initial  :: Initialized
    , used     :: Used
    , pure     :: Pure
    , offset   :: Offset
    , width    :: Width
    } deriving Show

type Initialized = Bool
type Used        = Bool
type Pure        = Bool
type Offset      = Int
type Width       = Int

--instance Show SymbolInfo where
--    show (SymbolInfo dt ct v sn dp i u p o) = showSN ++ showCT ++ showV ++ showDT ++ showDP ++ showU ++ showP ++ showO
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

emptySymbolInfo :: SymbolInfo
emptySymbolInfo = SymInfo
    { dataType = Void
    , category = CatVariable
    , value    = Nothing
    , scopeNum = -1
    , defPosn  = defaultPosn
    , initial  = False
    , used     = False
    , pure     = True
    , offset   = 0
    , width    = 0
    }

--------------------------------------------------------------------------------

data Value
    = ValInt      Int
    | ValFloat    Float
    | ValBool     Bool
    | ValChar     Char
    | ValType     DataType Width
    | ValFunction { parameters :: Seq (Lexeme DataType)
                  , impl       :: Maybe StBlock
                  , implPosn   :: Position
                  , implWidth  :: Width
                  }

instance Show Value where
    show val = " " ++ case val of
        ValInt v             ->  show v
        ValBool v            ->  show v
        ValChar v            ->  show v
        ValFloat v           ->  show v
        ValType dt w         ->  show dt ++ " [" ++ show w ++ "]"
        ValFunction p i ip w ->  "[" ++ showI ++ "] (" ++ showP ++ ") ->"
            where
                showP = drop 2 $ concatMap (\(Lex dt _) -> ", " ++ show dt) $ toList p
                showI = case i of
                    Just _  -> "implemented at " ++ show ip ++ " | " ++ show w
                    Nothing -> "NOT implemented"

----------------------------------------

{- |
    Symbol Table
-}
newtype SymbolTable = SymTable (DM.Map Identifier (Seq SymbolInfo))

instance Show SymbolTable where
    show (SymTable m) = "Symbol Table:\n" ++ DF.concatMap (++"\n") list''''
        where
            list :: [(Identifier, [SymbolInfo])]
            list = map (second DF.toList) $ DM.toList m
            list' :: [(Identifier, SymbolInfo)]
            list' = concatMap (\(var,ss) -> zip (repeat var) ss) list
            list'' :: [(ScopeNum, [(Identifier, SymbolInfo)])]
            list'' = map (\ls@((_,s):_) -> (scopeNum s,ls)) $ groupBy ((==) `on` (scopeNum . snd)) $ sortBy (compare `on` (scopeNum . snd)) list'
            list''' :: [(ScopeNum, [(Identifier, SymbolInfo)])]
            list''' = map (second (sortBy (compare `on` (defPosn . snd)))) list''
            list'''' :: [String]
            list'''' = map (\(sc,infos) -> show sc ++ " -> " ++ concat (map (\(iden,si) -> "\n\t\t" ++ iden ++ ":\t" ++ show si) infos)) list'''

{- |
    Empty symbol table
 -}
emptyTable :: SymbolTable
emptyTable = SymTable DM.empty

{- |
    Adds a symbol to the symbol table along with its information
 -}
insert :: Identifier -> SymbolInfo -> SymbolTable -> SymbolTable
insert vn info (SymTable m) = SymTable $ DM.alter func vn m
    where
        func Nothing  = Just $ singleton info
        func (Just x) = Just $ info <| x

{- |
    Looks up the symbol identified by var in the symbol table
 -}
lookup :: Identifier -> SymbolTable -> Maybe SymbolInfo
lookup var (SymTable m) = do
    is <- DM.lookup var m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

toListFilter :: SymbolTable -> ScopeNum -> Seq (Identifier, SymbolInfo)
toListFilter st@(SymTable m) sc = fromList $ foldl' func [] $ DM.keys m
    where
        func ls iden = maybe ls (\j -> (iden, j) : ls) $ maySI iden
        maySI iden   = lookupWithScope iden (singletonStack (Scope sc)) st

{- |
    Looks up the symbol identifier in the specified scope in the symbol table
 -}
lookupWithScope :: Identifier -> Stack Scope -> SymbolTable -> Maybe SymbolInfo
lookupWithScope var (Stack scopes) (SymTable m) = do
    is <- DM.lookup var m
    DF.msum $ map ((\sc -> DF.find ((sc==) . scopeNum) is) . serial) scopes

{- |
    Updates the SymbolInfo of a given identifier.
 -}
update :: Identifier -> (SymbolInfo -> SymbolInfo) -> SymbolTable -> SymbolTable
update var f (SymTable m) = SymTable $ DM.alter func var m
    where
        func mayIs = case mayIs of
            Just is -> case viewl is of
                i :< iss -> Just $ f i <| iss
                _        -> error $ "SymbolTable.update: No value to update for '" ++ var ++ "'"
            Nothing -> error $ "SymbolTable.update: Identifier '" ++ var ++ "' does not exist in symbol table"

{- |
    Updates the SymbolInfo of a given identifier, in a given scope.
-}
updateWithScope :: Identifier -> ScopeNum -> (SymbolInfo -> SymbolInfo) -> SymbolTable -> SymbolTable
updateWithScope var sc f (SymTable m) = SymTable $ DM.alter func var m
    where
        func = maybe failure (Just . DF.foldr foldFunc empty)
        failure = error $ "SymbolTable.update: Identifier '" ++ var ++ "' does not exist in symbol table"
        foldFunc i is = if scopeNum i == sc
            then f i <| is
            else   i <| is

{- |
    Returns all the variables
 -}
accessible :: SymbolTable -> Seq (Identifier, Seq SymbolInfo)
accessible (SymTable m) = fromList $ DM.toList m
