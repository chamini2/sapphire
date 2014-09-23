module Program
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

    -- From Position
    , Position(..)
    , defaultPosn

    -- From DataType
    , DataType(..)
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


import           DataType
import           Declaration
import           Expression
import           Identifier
import           Lexeme
import           Position
import           Statement

import           Data.Foldable as DF (concatMap)
import           Prelude       hiding (concatMap)

--------------------------------------------------------------------------------

newtype Program = Program StBlock

instance Show Program where
    show (Program sts) = concatMap ((++) "\n" . show) sts

--instance Show Program where
--    show (Program sts) = runPrinter $ printProgram
