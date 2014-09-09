module Language
    ( Program(..)

    , StBlock

    , Statement(..)
    , Declaration(..)
    , Category(..)
    , When(..)

    -- From Identifier
    , Identifier

    -- From Lexeme
    , Lexeme(..)

    -- From Position
    , Position(..)
    , defaultPosn

    -- From DataType
    , DataType(..)
    , Field
    --, getFields

    -- From Expression
    , Expression(..)
    , Binary(..)
    , Unary(..)
    --, binaryOperation
    --, unaryOperation
    ) where

import            DataType
import            Expression
import            Lexeme
import            Identifier
import            Position

--import           Control.Monad.Identity hiding (forM_, mapM_)
--import           Control.Monad.State    hiding (forM_, mapM_)
--import           Control.Monad.Writer   hiding (forM_, mapM_)
--import           Data.Char              (toLower)
--import           Data.Foldable          as DF (concat, concatMap, foldr, forM_, mapM_, toList)
import           Data.Foldable          as DF (concatMap)
--import           Data.Function          (on)
--import           Data.Functor           ((<$))
--import           Data.List              (intercalate)
--import           Data.Maybe             (fromJust)
--import           Data.Sequence          as DS (Seq, fromList, singleton)
import           Data.Sequence          as DS (Seq)
--import qualified Data.Typeable          as DT
import           Prelude                hiding (concat, concatMap, mapM_)

--------------------------------------------------------------------------------

newtype Program = Program StBlock

instance Show Program where
    show (Program sts) = concatMap ((++) "\n" . show) sts

--instance Show Program where
--    show (Program sts) = runPrinter $ printProgram

type StBlock    = Seq (Lexeme Statement)

----------------------------------------

data Statement
    -- Language
    = StNoop
--    | StAssign (Lexeme Access) (Lexeme Expression)
    -- Definitions
    | StVariableDeclaration (Lexeme Declaration)
    | StDeclarationList     (Seq (Lexeme Declaration))  -- Only used in Parser
    | StStructDefinition    (Lexeme DataType)
    -- Functions
    | StReturn        (Lexeme Expression)
    | StFunctionDef   (Lexeme Declaration) (Seq (Lexeme Declaration)) StBlock
    | StProcedureCall (Lexeme Identifier)  (Seq (Lexeme Expression))
    -- I/O
--    | StRead     (Lexeme Access)
    | StPrintList (Seq (Lexeme Expression))             -- Only used in Parser
    | StPrint     (Lexeme Expression)
    -- Conditional
    | StIf   (Lexeme Expression) StBlock StBlock
    | StCase (Lexeme Expression) (Seq (Lexeme When))      StBlock
    -- Loops
    | StLoop     StBlock (Lexeme Expression) StBlock
    | StFor      (Lexeme Identifier) (Lexeme Expression)  StBlock
    | StBreak
    | StContinue
    deriving Show

--instance Show Statement where
--    show = runPrinter . printStatement

----------------------------------------

data Declaration = Declaration (Lexeme Identifier) (Lexeme DataType) Category
    deriving (Show)

data Category
    = CatVariable
    | CatFunction
    | CatParameter
    | CatField
    | CatUserDef
    | CatLanguage   -- Language definitions
    deriving (Eq)

instance Show Category where
    show CatVariable  = "variable"
    show CatFunction  = "function"
    show CatParameter = "parameter"
    show CatField     = "field"
    show CatUserDef   = "data type"
    show CatLanguage  = "data type"

----------------------------------------

data When = When (Seq (Lexeme Expression)) StBlock
    deriving (Show)
