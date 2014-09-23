module DataType
    ( DataType(..)
    , Field

    , toIdentifier
    , isScalar
    , isValid
    , isArray
    , isStruct
    , arrayInnerDataType
    , fieldInStruct

    , DataTypeHistory
    , DataTypeZipper
    --, Thread
    , focusDataType
    , defocusDataType
    --, inDataType
    , backDataType
    , topDataType
    , deepDataType
    , putDataType
    ) where

import           Identifier
import           Lexeme

import           Data.Foldable as DF (toList, find)
import           Data.Functor  ((<$), (<$>))
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
            showFields fs      = " {" ++ intercalate ", " (toList $ fmap (\(i,d) -> lexInfo i ++ " : " ++ minimalDataType (lexInfo d)) fs) ++ "}"
            minimalDataType innerDt = case innerDt of
                Record idnL _ -> "record " ++ lexInfo idnL
                Union  idnL _ -> "union "  ++ lexInfo idnL
                _             -> show innerDt

----------------------------------------

type Field = (Lexeme Identifier, Lexeme DataType)

--------------------------------------------------------------------------------

toIdentifier :: DataType -> Identifier
toIdentifier dt = case dt of
    DataType idnL -> lexInfo idnL
    Int    -> "Int"
    Float  -> "Float"
    Bool   -> "Bool"
    Char   -> "Char"
    Range  -> "Range"
    Type   -> "Type"
    String -> "String"
    Record idnL _ -> lexInfo idnL
    Union  idnL _ -> lexInfo idnL
    Array dtL _   -> toIdentifier $ lexInfo dtL
    Void      -> "()"
    TypeError -> "Error"

----------------------------------------

isScalar :: DataType -> Bool
isScalar = flip elem [Int , Float , Bool , Char]

isValid :: DataType -> Bool
isValid dt = case dt of
    TypeError -> False
    _         -> True

isArray :: DataType -> Bool
isArray dt = case dt of
    Array _ _ -> True
    _         -> False

isStruct :: DataType -> Bool
isStruct dt = case dt of
    Record _ _ -> True
    Union  _ _ -> True
    _          -> False

arrayInnerDataType :: DataType -> DataType
arrayInnerDataType dt = case dt of
    Array dtL _ -> lexInfo dtL
    _           -> error "DataType.arrayInnerDataType: should not attempt to get inner DataType from a non-array DataType"

fieldInStruct :: DataType -> Identifier -> Maybe (Lexeme DataType)
fieldInStruct dt idn = case dt of
    Record _ flds -> snd <$> find ((idn==) . lexInfo . fst) flds
    Union  _ flds -> snd <$> find ((idn==) . lexInfo . fst) flds
    _             -> error "Language.fieldInStruct: should not attempt to get fields from a non-structure DataType"

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
