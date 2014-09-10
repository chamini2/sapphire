{-# LANGUAGE DeriveDataTypeable #-}
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

import           Data.Sequence (Seq, empty)
import qualified Data.Typeable as DT

type Initialized     = Bool
type Used            = Bool
type Pure            = Bool
type LanguageDefined = Bool
type Offset          = Int
type Width           = Int

data Symbol = SymInfo
                { dataType :: DataType
                , category :: Category
                , offset   :: Offset
                , width    :: Width
                , used     :: Used
                , scopeNum :: ScopeNum
                , defPosn  :: Position
                }
            | SymType
                { dataType :: DataType
                , langDef  :: LanguageDefined
                , width    :: Width
                , used     :: Used
                , scopeNum :: ScopeNum
                , defPosn  :: Position
                }
            | SymFunction
                { paramTypes :: Seq (Lexeme DataType)
                , returnType :: DataType
                , body       :: StBlock
                , width      :: Width
                , used       :: Used
                , scopeNum :: ScopeNum
                , defPosn    :: Position
                }
            deriving (Show, DT.Typeable)

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
    { dataType = Void
    , category = CatVariable
    , offset   = 0
    , width    = 0
    , used     = False
    , scopeNum = -1
    , defPosn  = defaultPosn
    }

emptySymType :: Symbol
emptySymType = SymType
    { dataType = Void
    , langDef  = False
    , width    = 0
    , used     = False
    , scopeNum = -1
    , defPosn  = defaultPosn
    }

emptySymFunction :: Symbol
emptySymFunction = SymFunction
    { paramTypes = empty
    , returnType = Void
    , body       = empty
    , width      = 0
    , used       = False
    , scopeNum   = -1
    , defPosn    = defaultPosn
    }

symbolCategory :: Symbol -> SymbolCategory
symbolCategory sym = case sym of
    SymInfo {}     -> CatInfo
    SymType {}     -> CatType
    SymFunction {} -> CatFunction
