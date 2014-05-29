{- |
    Symbol table based on the LeBlanc-Cook symbol table abstraction
 -}
module SymbolTable
    ( SymTable
    , emptyTable
    , insert
    , lookup
    , lookupWithScope
    , toListFilter
    , update
    , updateWithScope
    , accessible

    , SymInfo(..)
    , emptySymInfo
    , Init
    , Used
    , Pure
    , Offset

    , Scope(..)
    , ScopeNum

    , Value(..)

    , Stack
    , initialStack
    , singletonStack
    , peek
    , pop
    , push
    ) where

import           Language

import           Data.Foldable as DF
import qualified Data.Map      as DM
import           Data.Sequence as DS hiding (drop, update)
import           Prelude       as P  hiding (lookup, concatMap)

data SymInfo = SymInfo
    { dataType :: DataType
    , category :: Category
    , value    :: Maybe Value
    , scopeNum :: ScopeNum
    , defPosn  :: Position
    , initial  :: Init
    , used     :: Used
    , pure     :: Pure
    , offset   :: Offset
    }

type Init   = Bool
type Used   = Bool
type Pure   = Bool
type Offset = Int

instance Show SymInfo where
    show (SymInfo dt ct v sn dp i u p o) = showSN ++ showCT ++ showV ++ showDT ++ showDP ++ showU ++ showP ++ showO
        where
            showSN = "Scope: " ++ show sn ++ ", "
            showCT = show ct ++ " | "
            showDT = show dt ++ " "
            showV  = showI ++ maybe "" show v ++ " "
                where
                    showI  = "(" ++ (if i then "init" else "NOT init") ++ ")"
            showDP = show dp
            showU  = " (" ++ (if u then "used" else "NOT used") ++ ")"
            showP  = " (" ++ (if p then "pure" else "impure") ++ ")"
            showO  = show o

emptySymInfo :: SymInfo
emptySymInfo = SymInfo
    { dataType = Void
    , category = CatVariable
    , value    = Nothing
    , scopeNum = -1
    , defPosn  = (0, 0)
    , initial  = False
    , used     = False
    , pure     = True
    , offset   = 0
    }

--------------------------------------------------------------------------------

data Scope = Scope { serial :: ScopeNum } deriving (Show)

{- |
    The outermost scope has a default value of 0
-}
initialScope :: Scope
initialScope = Scope { serial = 0 }

type ScopeNum = Int

--------------------------------------------------------------------------------

data Value
    = ValInt      Int
    | ValBool     Bool
    | ValChar     Char
    | ValFloat    Float
    | ValStruct   SymTable
    | ValFunction { parameters :: Seq (Lexeme DataType)
                  , impl       :: Maybe StBlock
                  , implPosn   :: Position
                  }

instance Show Value where
    show (ValInt v)          = show v
    show (ValBool v)         = show v
    show (ValChar v)         = show v
    show (ValFloat v)        = show v
    show (ValStruct s)       = show s
    show (ValFunction p i _) = "[" ++ showI ++ "] (" ++ showP ++ ") ->"
        where
            showP = drop 2 $ concatMap (\(Lex dt _) -> ", " ++ show dt) $ toList p
            showI = case i of
                Just _  -> "implemented"
                Nothing -> "NOT implemented"

----------------------------------------

{- |
    Symbol Table
-}
newtype SymTable = SymTable (DM.Map Identifier (Seq SymInfo))

instance Show SymTable where
    show (SymTable m) = concatMap shower $ DM.toList m
        where
            shower (var, info) = var ++ " -> " ++ showInfo info ++ "\n"
            showInfo = concatMap ((++) "\n\t" . show) . DF.toList
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
    where f Nothing  = Just $ singleton info
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

toListFilter :: SymTable -> ScopeNum -> Seq (Identifier, SymInfo)
toListFilter st@(SymTable m) sc = fromList $ foldl' hack [] $ DM.keys m
    where
        hack ls iden = maybe ls (\j -> (iden, j) : ls) $ maySI iden
        maySI iden   = lookupWithScope iden (singletonStack (Scope sc)) st

{- |
    Looks up the symbol identifier in the specified scope in the symbol table
 -}
lookupWithScope :: Identifier -> Stack Scope -> SymTable -> Maybe SymInfo
lookupWithScope var (Stack scopes) (SymTable m) = do
    is <- DM.lookup var m
    DF.msum $ map ((\sc -> DF.find ((sc==) . scopeNum) is) . serial) scopes

{- |
    Updates the SymInfo of a given identifier.
 -}
update :: Identifier -> (SymInfo -> SymInfo) -> SymTable -> SymTable
update var f (SymTable m) = SymTable $ DM.alter func var m
    where
        func mayIs = case mayIs of
            Just is -> case viewl is of
                i :< iss -> Just $ f i <| iss
                _        -> error $ "SymbolTable.update: No value to update for '" ++ var ++ "'"
            Nothing -> error $ "SymbolTable.update: Identifier '" ++ var ++ "' does not exist in symbol table"

{- |
    Updates the SymInfo of a given identifier, in a given scope.
-}
updateWithScope :: Identifier -> ScopeNum -> (SymInfo -> SymInfo) -> SymTable -> SymTable
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
accessible :: SymTable -> Seq (Identifier, Seq SymInfo)
accessible (SymTable m) = fromList $ DM.toList m

--------------------------------------------------------------------------------

newtype Stack a = Stack [a]

instance Show a => Show (Stack a) where
    show (Stack s) = drop 1 $ concatMap ((++) "\n\t" . show) s

instance Functor Stack where
    fmap f (Stack s) = Stack $ map f s

instance DF.Foldable Stack where
    foldr f b (Stack s) = P.foldr f b s

{- |
    Shows the first element in the stack, if there are any, without popping it.
-}
peek :: Stack a -> a
peek (Stack [])      = error "SymbolTable.peek: Empty stack"
peek (Stack (x : _)) = x

{- |
    Pushes an element to the stack.
-}
push :: a -> Stack a -> Stack a
push element (Stack s) = Stack $ element : s

{- |
    Pops an element from the stack.
-}
pop :: Stack a -> (a, Stack a)
pop (Stack [])      = error "SymbolTable.pop: Empty stack"
pop (Stack (x : s)) = (x, Stack s)

{- |
    The scope stack has the inital scope by default.
-}
initialStack :: Stack Scope
initialStack = Stack [ initialScope ]

singletonStack :: a -> Stack a
singletonStack n = Stack [ n ]
