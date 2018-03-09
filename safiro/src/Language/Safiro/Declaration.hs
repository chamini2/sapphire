module Language.Safiro.Declaration
    ( Declaration(..)
    , Category(..)
    ) where

import           Language.Safiro.DataType
import           Language.Safiro.Identifier
import           Language.Safiro.Lexeme

data Declaration = Declaration
    { dclIdentifier :: Lexeme Identifier
    , dclDataType   :: Lexeme DataType
    , dclCategory   :: Category
    }


instance Show Declaration where
    show (Declaration idnL dtL _) = lexInfo idnL ++ " : " ++ show (lexInfo dtL)

data Category
    = CatVariable
    | CatConstant
    | CatParameter
    | CatField
    deriving (Eq)

instance Show Category where
    show CatVariable  = "variable"
    show CatConstant  = "constant"
    show CatParameter = "parameter"
    show CatField     = "field"
