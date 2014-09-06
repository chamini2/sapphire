{- |
    Symbol table based on the LeBlanc-Cook symbol table abstraction
 -}
module SymbolTable
    ( SymTable
    , emptyTable
    , initialTable
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

--import           Control.Arrow (second)
--import           Data.Foldable as DF
--import           Data.Function (on)
--import           Data.List     (groupBy, sortBy)
--import qualified Data.Map      as DM
--import           Data.Sequence as DS hiding (zip, drop, update, sortBy)
import           Prelude       as P hiding (concatMap, lookup, concat)

data SymInfo = SymInfo
    { dataType :: DataType
    , category :: Category
    , value    :: Maybe Value
    , scopeNum :: ScopeNum
    , defPosn  :: Position
    , init     :: Initialized
    , used     :: Used
    , pure     :: Pure
    , offset   :: Offset
    }

type Initialized = Bool
type Used        = Bool
type Pure        = Bool
type Offset      = Int

