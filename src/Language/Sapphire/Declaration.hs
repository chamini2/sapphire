module Language.Sapphire.Declaration
    ( Declaration(..)
    , Category(..)
    ) where

import           Language.Sapphire.DataType
import           Language.Sapphire.Identifier
import           Language.Sapphire.Lexeme

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
