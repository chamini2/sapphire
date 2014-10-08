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
    , pure
    , Position(..)
    , defaultPosn
    , row
    , col

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
    , isComparable

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

--------------------------------------------------------------------------------

newtype Program = Program StBlock
