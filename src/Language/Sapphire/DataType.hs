module Language.Sapphire.DataType
    ( DataType(..)
    , Field

    , toIdentifier
    , isVoid
    , isScalar
    , isValid
    , isArray
    , isStruct
    , arrayInnerDataType

    , DataTypeHistory
    , DataTypeZipper
    --, Thread
    , focusDataType
    , defocusDataType
    --, inDataType
    -- , backDataType
    , topDataType
    , deepDataType
    , putDataType
    ) where

import           Language.Sapphire.Identifier
import           Language.Sapphire.Lexeme

import           Data.Foldable                (toList)
import           Data.Functor                 ((<$))
import           Data.List                    (intercalate)
import           Data.Maybe                   (fromJust)
import           Data.Sequence                (Seq)

data DataType
    = DataType (Lexeme Identifier)
    | Int | Float | Bool | Char | Range | Type
    | String
    | Record (Lexeme Identifier)
    | Union  (Lexeme Identifier)
    | Array  (Lexeme DataType)   (Lexeme Int)
    | Void | TypeError                          -- For compiler use
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
        Record idnL     -> "record " ++ lexInfo idnL
        Union  idnL     -> "union "  ++ lexInfo idnL
        Array  dtL sizL -> show (lexInfo dtL) ++ "[" ++ show (lexInfo sizL) ++ "]"
        Void            -> "()"
        TypeError       -> error "DataType TypeError should never be shown"
        where
            showFields :: Seq Field -> String
            showFields flds = " {" ++ intercalate ", " (toList $ fmap (\(i,d) -> lexInfo i ++ " : " ++ show d) flds) ++ "}"

----------------------------------------

type Field = (Lexeme Identifier, Lexeme DataType)

--------------------------------------------------------------------------------

-- For lookups in the SymbolTable
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
    Record idnL -> lexInfo idnL
    Union  idnL -> lexInfo idnL
    Array dtL _ -> toIdentifier $ lexInfo dtL
    Void      -> "()"
    TypeError -> "Error"

----------------------------------------

isVoid :: DataType -> Bool
isVoid = (== Void)

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
    Record _   -> True
    Union  _   -> True
    _          -> False

arrayInnerDataType :: DataType -> DataType
arrayInnerDataType dt = case dt of
    Array dtL _ -> lexInfo dtL
    _           -> error "DataType.arrayInnerDataType: should not attempt to get inner DataType from a non-array DataType"

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

type DataTypeZipper = (Lexeme DataType, Int, Thread)

----------------------------------------

focusDataType :: Lexeme DataType -> DataTypeZipper
focusDataType dtL = (dtL, 1, [])

defocusDataType :: DataTypeZipper -> (Lexeme DataType, Int)
defocusDataType (dtL, dim, _) = (dtL, dim)

inDataType :: DataTypeZipper -> Maybe DataTypeZipper
inDataType (dtL, dim, thrd) = case lexInfo dtL of
    Array inDtL sizL -> Just (inDtL, dim * lexInfo sizL, (HistoryDataType sizL <$ dtL) : thrd)
    _                -> Nothing

backDataType :: DataTypeZipper -> Maybe DataTypeZipper
backDataType (dtL, dim, thrd) = case thrd of
    []                                          -> Nothing
    hstL@(Lex (HistoryDataType sizL) _) : hstLs -> Just (Array dtL sizL <$ hstL, dim `div` lexInfo sizL, hstLs)

topDataType :: DataTypeZipper -> DataTypeZipper
topDataType zpp@(_, _, thrd) = case thrd of
    []    -> zpp
    _ : _ -> topDataType $ fromJust $ backDataType zpp

deepDataType :: DataTypeZipper -> DataTypeZipper
deepDataType zpp@(dtL, _, _) = case lexInfo dtL of
    Array _ _ -> deepDataType $ fromJust $ inDataType zpp
    _         -> zpp

putDataType :: Lexeme DataType -> DataTypeZipper -> DataTypeZipper
putDataType dtL (_, dim, thrd) = (dtL, dim, thrd)
