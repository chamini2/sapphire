{-# LANGUAGE LambdaCase #-}
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

import           Data.Functor                 ((<$))
import           Data.Function                (on)
import           Data.Maybe                   (fromJust)

data DataType
    = DataType (Lexeme Identifier)                  -- For compiler use
    | Int | Float | Bool | Char | Range | Type
    | String
    | Record { structIdentifier :: (Lexeme Identifier) }
    | Union  { structIdentifier :: (Lexeme Identifier) }
    | Array     (Lexeme DataType) (Lexeme Int)
    | ArraySign (Lexeme DataType)                   -- For signatures
    | Void | TypeError                              -- For compiler use
    deriving (Ord)

instance Show DataType where
    show = \case
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
        Array  dtL sizL -> "[" ++ show (lexInfo sizL) ++ "]" ++  show (lexInfo dtL)
        ArraySign  dtL  -> "[]" ++  show (lexInfo dtL)
        Void            -> "()"
        TypeError       -> error "DataType.DataType.show: TypeError should never be shown"

instance Eq DataType where
    a == b = case (a,b) of
        (DataType idnAL, DataType idnBL)     -> comp idnAL idnBL
        (Int, Int)                           -> True
        (Float, Float)                       -> True
        (Bool, Bool)                         -> True
        (Char, Char)                         -> True
        (Range, Range)                       -> True
        (Type, Type)                         -> True
        (String, String)                     -> True
        (Record idnAL, Record idnBL)         -> comp idnAL idnBL
        (Union  idnAL, Union  idnBL)         -> comp idnAL idnBL
        (Array dtAL sizAL, Array dtBL sizBL) -> (comp dtAL dtBL) && (comp sizAL sizBL)

        (ArraySign dtAL, Array dtBL _  )     -> (comp dtAL dtBL)
        (Array dtAL _  , ArraySign dtBL)     -> (comp dtAL dtBL)
        (ArraySign dtAL, ArraySign dtBL)     -> (comp dtAL dtBL)

        (Void, Void)                         -> True
        (TypeError, TypeError)               -> True
        _                                    -> False
        where
            comp :: Eq a => Lexeme a -> Lexeme a -> Bool
            comp = (==) `on` lexInfo

----------------------------------------

type Field = (Lexeme Identifier, Lexeme DataType)

--------------------------------------------------------------------------------

-- For lookups in the SymbolTable
toIdentifier :: DataType -> Identifier
toIdentifier = \case
    DataType idnL -> lexInfo idnL
    Int           -> "Int"
    Float         -> "Float"
    Bool          -> "Bool"
    Char          -> "Char"
    Range         -> "Range"
    Type          -> "Type"
    String        -> "String"
    Record idnL   -> lexInfo idnL
    Union  idnL   -> lexInfo idnL
    Array dtL _   -> toIdentifier $ lexInfo dtL
    ArraySign dtL -> toIdentifier $ lexInfo dtL
    Void          -> "()"
    TypeError     -> "Error"

----------------------------------------

isVoid :: DataType -> Bool
isVoid = (== Void)

isScalar :: DataType -> Bool
isScalar = flip elem [Int , Float , Bool , Char]

isValid :: DataType -> Bool
isValid = (/= TypeError)

isArray :: DataType -> Bool
isArray = \case
    Array _ _   -> True
    ArraySign _ -> True
    _           -> False

isStruct :: DataType -> Bool
isStruct = \case
    Record _   -> True
    Union  _   -> True
    _          -> False

arrayInnerDataType :: DataType -> DataType
arrayInnerDataType = \case
    Array dtL _   -> lexInfo dtL
    ArraySign dtL -> lexInfo dtL
    _             -> error "DataType.arrayInnerDataType: should not attempt to get inner DataType from a non-array DataType"

--------------------------------------------------------------------------------

{-
 - derivating the DataTypeHistory data
 -
 - DataType = dt
 - DataTypeHistory = dt'
 -
 - dt = identifier +
 -      1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
 -      (identifier * fields) + (identifier * fields) +
 -      (dt * int) + dt
 -
 - identifier = A
 - fields = B
 - int = C
 -
 - dt = A + 9 + 2 * A * B + dt * C + dt
 -
 - dt' = C + 1
 -}

data DataTypeHistory = HistoryDataTypeArray (Lexeme Int)
                     | HistoryDataTypeArraySign

type Thread = [Lexeme DataTypeHistory]

type DataTypeZipper = (Lexeme DataType, Int, Thread)

----------------------------------------

focusDataType :: Lexeme DataType -> DataTypeZipper
focusDataType dtL = (dtL, 1, [])

defocusDataType :: DataTypeZipper -> (Lexeme DataType, Int)
defocusDataType (dtL, dim, _) = (dtL, dim)

inDataType :: DataTypeZipper -> Maybe DataTypeZipper
inDataType (dtL, dim, thrd) = case lexInfo dtL of
    Array inDtL sizL -> Just (inDtL, dim * lexInfo sizL, (HistoryDataTypeArray sizL <$ dtL) : thrd)
    ArraySign inDtL  -> Just (inDtL, 4                 , (HistoryDataTypeArraySign  <$ dtL) : thrd)
    _                -> Nothing

backDataType :: DataTypeZipper -> Maybe DataTypeZipper
backDataType (dtL, dim, thrd) = case thrd of
    []                                          -> Nothing
    hstL@(Lex (HistoryDataTypeArray sizL) _) : hstLs -> Just (Array dtL sizL <$ hstL, dim `div` lexInfo sizL, hstLs)
    hstL@(Lex (HistoryDataTypeArraySign ) _) : hstLs -> Just (ArraySign dtL  <$ hstL, 4                     , hstLs)

topDataType :: DataTypeZipper -> DataTypeZipper
topDataType zpp@(_, _, thrd) = if null thrd
    then zpp
    else topDataType $ fromJust $ backDataType zpp

deepDataType :: DataTypeZipper -> DataTypeZipper
deepDataType zpp@(dtL, _, _) = if isArray (lexInfo dtL)
    then deepDataType $ fromJust $ inDataType zpp
    else zpp

putDataType :: Lexeme DataType -> DataTypeZipper -> DataTypeZipper
putDataType dtL (_, dim, thrd) = (dtL, dim, thrd)
