{-# LANGUAGE DeriveDataTypeable #-}
module DataType
    ( DataType(..)
    , Field
    , getFields
    ) where

import           Lexeme
import           Identifier

import qualified Data.Data              as DD (Data)
import           Data.Foldable          as DF (toList)
import           Data.List              (intercalate)
import           Data.Sequence          as DS (Seq)
import qualified Data.Typeable          as DT (Typeable)

data DataType
    = Int | Float | Bool | Char | Range | Type
    | String
    | Record (Lexeme Identifier) (Seq Field)
    | Union  (Lexeme Identifier) (Seq Field)
--    | Array   (Lexeme DataType) (Lexeme Int) Width
    | UserDef (Lexeme Identifier)
    | Void | TypeError  -- For compiler use
    deriving (Ord, Eq, DT.Typeable, DD.Data)

instance Show DataType where
    show dt = case dt of
        Int            -> "Int"
        Float          -> "Float"
        Bool           -> "Bool"
        Char           -> "Char"
        String         -> "String"
        Range          -> "Range"
        Type           -> "Type"
        Record iden fs -> "Record " ++ lexInfo iden ++ showFields fs
        Union  iden fs -> "Union "  ++ lexInfo iden ++ showFields fs
--        Array aDtL _ w -> show (lexInfo aDtL) ++ "[" ++ show w ++ "]"
        UserDef idenL  -> lexInfo idenL
        Void           -> "()"
        TypeError      -> error "DataType TypeError should never be shown"
        where
            showFields fs = " {" ++ intercalate ", " (toList $ fmap (\(i,d) -> lexInfo i ++ " : " ++ show (lexInfo d)) fs) ++ "}"

type Field = (Lexeme Identifier, Lexeme DataType)

getFields :: DataType -> Seq Field
getFields dt = case dt of
    Record _ fields -> fields
    Union  _ fields -> fields
    _               -> error "Language.getFields: should not attempt to get fields from non-structure DataType"
