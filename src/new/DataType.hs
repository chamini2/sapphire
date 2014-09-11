module DataType
    ( DataType(..)
    , Field
    , getFields

    , DataTypeHistory
    , DataTypeZipper
    --, Thread
    , focusDataType
    , defocusDataType
    --, inDataType
    --, backDataType
    , topDataType
    , deepDataType
    , putDataType
    ) where

import           Identifier
import           Lexeme

import           Data.Foldable as DF (toList)
import           Data.Functor  ((<$))
import           Data.List     (intercalate)
import           Data.Maybe    (fromJust)
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
        DataType idnL   -> "DataType " ++ lexInfo idnL
        Int             -> "Int"
        Float           -> "Float"
        Bool            -> "Bool"
        Char            -> "Char"
        String          -> "String"
        Range           -> "Range"
        Type            -> "Type"
        Record idnL fs  -> "record " ++ lexInfo idnL ++ showFields fs
        Union  idnL fs  -> "union "  ++ lexInfo idnL ++ showFields fs
        Array  dtL sizL -> show (lexInfo dtL) ++ "[" ++ show (lexInfo sizL) ++ "]"
        Void            -> "()"
        TypeError       -> error "DataType TypeError should never be shown"
        where
            showFields fs = " {" ++ intercalate ", " (toList $ fmap (\(i,d) -> lexInfo i ++ " : " ++ show (lexInfo d)) fs) ++ "}"

type Field = (Lexeme Identifier, Lexeme DataType)

getFields :: DataType -> Seq Field
getFields dt = case dt of
    Record _ flds -> flds
    Union  _ flds -> flds
    _               -> error "Language.getFields: should not attempt to get fields from non-structure DataType"

--------------------------------------------------------------------------------

{-
 - deriving the DataTypeHistory data
 -
 - DataType = dt
 - DataTypeHistory = dt'
 -
 - dt = identifier +
 -      1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
 -      (identifier * fields) + (identifier * fields) +
 -      (dt * int) +
 -
 - identifier = A
 - fields = B
 - int = C
 -
 - dt = A + 9 + 2 * A * B + dt * C
 -
 - dt' = C
 -}

data DataTypeHistory = HistoryDataType (Lexeme Int)
    deriving (Show)

type Thread = [Lexeme DataTypeHistory]

type DataTypeZipper = (Lexeme DataType, Thread)

----------------------------------------

focusDataType :: Lexeme DataType -> DataTypeZipper
focusDataType dtL = (dtL, [])

defocusDataType :: DataTypeZipper -> Lexeme DataType
defocusDataType = fst

inDataType :: DataTypeZipper -> Maybe DataTypeZipper
inDataType (hstL, thrd) = case lexInfo hstL of
    Array dtL sizL -> Just (dtL, (HistoryDataType sizL <$ hstL) : thrd)
    _              -> Nothing

backDataType :: DataTypeZipper -> Maybe DataTypeZipper
backDataType (dtL, thrd) = case thrd of
    []                                          -> Nothing
    hstL@(Lex (HistoryDataType sizL) _) : hstLs -> Just (Array dtL sizL <$ hstL, hstLs)

topDataType :: DataTypeZipper -> DataTypeZipper
topDataType zpp = case snd zpp of
    []    -> zpp
    _ : _ -> topDataType $ fromJust $ backDataType zpp

deepDataType :: DataTypeZipper -> DataTypeZipper
deepDataType zpp = case lexInfo $ fst zpp of
    Array _ _ -> deepDataType $ fromJust $ inDataType zpp
    _         -> zpp

putDataType :: Lexeme DataType -> DataTypeZipper -> DataTypeZipper
putDataType dtL (_, thrd) = (dtL, thrd)
