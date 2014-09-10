module DataType
    ( DataType(..)
    , Field
    , getFields
    ) where

import           Identifier
import           Lexeme

import           Data.Foldable as DF (toList)
import           Data.List     (intercalate)
import           Data.Sequence as DS (Seq)

data DataType
    = DataType (Lexeme Identifier)
    | Int | Float | Bool | Char | Range | Type
    | String
    | Record (Lexeme Identifier) (Seq Field)
    | Union  (Lexeme Identifier) (Seq Field)
    | Array  (Lexeme DataType)   (Lexeme Int)
    | Void | TypeError  -- For compiler use
    deriving (Ord, Eq)

instance Show DataType where
    show dt = case dt of
        DataType idnL  -> "DataType " ++ lexInfo idnL
        Int            -> "Int"
        Float          -> "Float"
        Bool           -> "Bool"
        Char           -> "Char"
        String         -> "String"
        Range          -> "Range"
        Type           -> "Type"
        Record idnL fs -> "Record " ++ lexInfo idnL ++ showFields fs
        Union  idnL fs -> "Union "  ++ lexInfo idnL ++ showFields fs
        Array  dtL siz -> show (lexInfo dtL) ++ "[" ++ show (lexInfo siz) ++ "]"
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
