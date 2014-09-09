{-# LANGUAGE DeriveDataTypeable #-}
module Language
    ( Program(..)

    , Identifier
    , StBlock

    , DataType(..)

    , Field

    , Statement(..)
    , Declaration(..)
    , Category(..)

    , Expression(..)
    , Binary(..)
    , Unary(..)
    ) where

import            Position ()
import            Lexeme

--import           Control.Monad.Identity hiding (forM_, mapM_)
--import           Control.Monad.State    hiding (forM_, mapM_)
--import           Control.Monad.Writer   hiding (forM_, mapM_)
--import           Data.Char              (toLower)
--import qualified Data.Data              as DD
import qualified Data.Data              as DD (Data)
--import           Data.Foldable          as DF (concat, concatMap, foldr, forM_, mapM_, toList)
import           Data.Foldable          as DF (concatMap, toList)
--import           Data.Function          (on)
--import           Data.Functor           ((<$))
import           Data.List              (intercalate)
--import           Data.Maybe             (fromJust)
--import           Data.Sequence          as DS (Seq, fromList, singleton)
import           Data.Sequence          as DS (Seq, fromList)
--import qualified Data.Typeable          as DT
import qualified Data.Typeable          as DT (Typeable)
import           Prelude                hiding (concat, concatMap, mapM_)

--------------------------------------------------------------------------------

newtype Program = Program StBlock

instance Show Program where
    show (Program sts) = concatMap ((++) "\n" . show) sts

--instance Show Program where
--    show (Program sts) = runPrinter $ printProgram

type Identifier = String
type StBlock    = Seq (Lexeme Statement)

----------------------------------------

data DataType
    = Int | Float | Bool | Char | Range | Type
--    | String Width
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
--        String _       -> "String"
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
--    | StFunctionImp   (Lexeme Identifier)  (Seq (Lexeme Identifier)) StBlock
    | StProcedureCall (Lexeme Identifier)  (Seq (Lexeme Expression))
    -- I/O
--    | StRead     (Lexeme Access)
    | StPrintList (Seq (Lexeme Expression))             -- Only used in Parser
    | StPrint     (Lexeme Expression)
    -- Conditional
    | StIf   (Lexeme Expression) StBlock StBlock
--    | StCase (Lexeme Expression) (Seq (Lexeme When))      StBlock
    -- Loops
--    | StLoop     StBlock (Lexeme Expression) StBlock
--    | StFor      (Lexeme Identifier) (Lexeme Expression)  StBlock
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

--data When = When (Seq (Lexeme Expression)) StBlock
--    deriving (Show)

----------------------------------------

data Expression
    -- Variable
--    = Variable (Lexeme Access)
    -- Function call
--    | FunctionCall (Lexeme Identifier) (Seq (Lexeme Expression))
    -- Literals
--    | LitInt    (Lexeme Int)
    = LitInt    (Lexeme Int)
    | LitFloat  (Lexeme Float)
    | LitBool   (Lexeme Bool)
    | LitChar   (Lexeme Char)
    | LitString (Lexeme String)
--    | LitRange  (Lexeme Range)
    -- Operators
    | ExpBinary (Lexeme Binary) (Lexeme Expression) (Lexeme Expression)
    | ExpUnary  (Lexeme Unary)  (Lexeme Expression)
    deriving (Eq, Ord, DT.Typeable, DD.Data, Show)

--instance Show Expression where
--    show = runPrinter . printExpression

data Binary
    = OpPlus  | OpMinus   | OpTimes | OpDivide | OpModulo | OpPower   | OpFromTo
    | OpEqual | OpUnequal | OpLess  | OpLessEq | OpGreat  | OpGreatEq | OpBelongs
    | OpOr    | OpAnd
    deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show Binary where
    show op = case op of
        OpPlus    -> "+"
        OpMinus   -> "-"
        OpTimes   -> "*"
        OpDivide  -> "/"
        OpModulo  -> "%"
        OpPower   -> "^"
        OpFromTo  -> ".."
        OpOr      -> "or"
        OpAnd     -> "and"
        OpEqual   -> "=="
        OpUnequal -> "/="
        OpLess    -> "<"
        OpLessEq  -> "<="
        OpGreat   -> ">"
        OpGreatEq -> ">="
        OpBelongs -> "@"

--instance Show Binary where
--    show op = case op of
--        OpPlus    -> "arithmetic addition"
--        OpMinus   -> "arithmetic substraction"
--        OpTimes   -> "arithmetic multiplication"
--        OpDivide  -> "arithmetic division"
--        OpModulo  -> "arithmetic Modulo"
--        OpPower   -> "arithmetic power"
--        OpFromTo  -> "range construction operator"
--        OpOr      -> "logical disjunction"
--        OpAnd     -> "logical conjunction"
--        OpEqual   -> "equal to"
--        OpUnequal -> "not equal to"
--        OpLess    -> "less than"
--        OpLessEq  -> "less than or equal to"
--        OpGreat   -> "greater than"
--        OpGreatEq -> "greater than or equal to"
--        OpBelongs -> "belongs to Range"

binaryOperation :: Binary -> Seq ((DataType, DataType), DataType)
binaryOperation op = fromList $ case op of
    OpPlus    -> arithmetic
    OpMinus   -> arithmetic
    OpTimes   -> arithmetic
    OpDivide  -> arithmetic
    OpModulo  -> arithmetic
    OpPower   -> [ ((Int, Int), Int), ((Float, Int), Float) ]
    OpFromTo  -> [ ((Int, Int), Range) ]
    OpOr      -> boolean
    OpAnd     -> boolean
    OpEqual   -> everything
    OpUnequal -> everything
    OpLess    -> arithmeticCompare
    OpLessEq  -> arithmeticCompare
    OpGreat   -> arithmeticCompare
    OpGreatEq -> arithmeticCompare
    OpBelongs -> [ ((Int, Range), Bool) ]
    where
        everything = arithmetic ++ boolean
        arithmetic = [((Int, Int), Int), ((Float, Float), Float)]
        boolean = [ ((Bool, Bool), Bool) ]
        arithmeticCompare = zip numbers $ repeat Bool
        numbers = [(Int, Int), (Float, Float)]

data Unary = OpNegate | OpNot
    deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show Unary where
    show op = case op of
        OpNegate -> "-"
        OpNot    -> "not"

--instance Show Unary where
--    show op = case op of
--        OpNegate -> "arithmetic negation"
--        OpNot    -> "logical negation"

unaryOperation :: Unary -> Seq (DataType, DataType)
unaryOperation op = fromList $ case op of
    OpNegate -> [(Int, Int), (Float, Float)]
    OpNot    -> [(Bool, Bool)]

