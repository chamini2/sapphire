module Language.Sapphire.Program
    ( Program(..)

    -- From Statement
    , Statement(..)
    , StBlock
    , When(..)
    , Signature(..)

    -- From Declaration
    , Declaration(..)
    , Category(..)

    -- From Identifier
    , Identifier

    -- From Lexeme
    , Lexeme(..)
    , fillLex
    , Position(..)
    , defaultPosn

    -- From DataType
    , DataType(..)
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

    -- From Expression
    , Expression(..)
    , Binary(..)
    , Unary(..)
    , binaryOperation
    , unaryOperation

    , Access(..)
    , AccessHistory(..)

    , AccessZipper
    --, Thread
    , focusAccess
    , defocusAccess
    --, inArrayAccess
    --, inStructAccess
    --, inAccess
    , backAccess
    , topAccess
    , deepAccess
    ) where


import           Language.Sapphire.DataType
import           Language.Sapphire.Declaration
import           Language.Sapphire.Expression
import           Language.Sapphire.Identifier
import           Language.Sapphire.Lexeme
import           Language.Sapphire.Statement

-- import           Data.Foldable                 (concatMap)
-- import           Prelude                       hiding (concatMap)

--------------------------------------------------------------------------------

newtype Program = Program StBlock

-- instance Show Program where
--     show (Program sts) = concatMap ((++) "\n" . show) sts
