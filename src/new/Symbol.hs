module Symbol
    ( Symbol(..)
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
    ) where

import           Program
import           Scope
import           Stack

import           Data.Sequence (Seq, empty)

type Initialized     = Bool
type Used            = Bool
type Pure            = Bool
type LanguageDefined = Bool
type Offset          = Int
type Width           = Int

data Symbol = SymInfo
                { dataType   :: Lexeme DataType
                , category   :: Category
                , offset     :: Offset
                , width      :: Width
                , used       :: Used
                , scopeNum   :: ScopeNum
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            | SymType
                { dataType   :: Lexeme DataType
                , langDef    :: LanguageDefined
                , width      :: Width
                , used       :: Used
                , scopeNum   :: ScopeNum
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            | SymFunction
                { paramTypes :: Seq (Lexeme DataType)
                , returnType :: Lexeme DataType
                , body       :: StBlock
                , width      :: Width
                , used       :: Used
                , scopeNum   :: ScopeNum
                , scopeStack :: Stack Scope
                , defPosn    :: Position
                }
            deriving (Show)

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

emptySymInfo :: Symbol
emptySymInfo = SymInfo
    { dataType   = fillLex Void
    , category   = CatVariable
    , offset     = 0
    , width      = 0
    , used       = False
    , scopeNum   = -1
    , scopeStack = singletonStack outerScope
    , defPosn    = defaultPosn
    }

emptySymType :: Symbol
emptySymType = SymType
    { dataType   = fillLex Void
    , langDef    = False
    , width      = 0
    , used       = False
    , scopeNum   = -1
    , scopeStack = singletonStack outerScope
    , defPosn    = defaultPosn
    }

emptySymFunction :: Symbol
emptySymFunction = SymFunction
    { paramTypes = empty
    , returnType = fillLex Void
    , body       = empty
    , width      = 0
    , used       = False
    , scopeNum   = -1
    , scopeStack = singletonStack outerScope
    , defPosn    = defaultPosn
    }

----------------------------------------

symbolCategory :: Symbol -> SymbolCategory
symbolCategory sym = case sym of
    SymInfo {}     -> CatInfo
    SymType {}     -> CatType
    SymFunction {} -> CatFunction
