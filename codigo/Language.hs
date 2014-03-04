{-# LANGUAGE DeriveDataTypeable #-}
module Language where

import           Data.Typeable          (Typeable (..))

import           Control.Monad.Error    (Error (..), ErrorT (..), runErrorT)
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS      (RWST (..), runRWST)


type Program  = [Checker Statement]
type Identifier = String

data DataType = Void | Int | Float | Bool | Char | String | Range | Type -- | Array
    deriving (Show, Typeable, Eq)

data Declaration = Declaration Identifier DataType
    deriving (Show, Typeable)

data Statement
    = StNoop
    | StAssign Identifier Expression

    --Definitions
    | StDeclaration [Declaration]
    | StReturn Expression

    -- I/O
    | StRead  [Identifier]
    | StPrint [Expression]

    -- Conditional
    | StIf   Expression [Statement] [Statement]
    | StCase Expression [Case] [Statement]

    -- Loops
    | StWhile Expression [Statement]
    | StFor   Identifier Expression [Statement]
    | StBreak
    | StContinue
    deriving (Show, Typeable)

data Case = Case Expression [Statement]
    deriving (Show, Typeable)

data Binary
    = OpPlus  | OpMinus | OpTimes | OpDivide | OpModulo | OpPower | OpFromTo
    | OpOr    | OpAnd
    | OpEqual | OpUnEqual | OpLess | OpLessEq | OpGreat | OpGreatEq | OpBelongs
    deriving (Show)

data Unary = OpNegate | OpNot
    deriving (Show)

data Range = FromTo Expression Expression
    deriving (Show)

data Expression
    -- Variable
    = Variable Identifier
    -- Literals
    | LitInt    Int
    | LitFloat  Float
    | LitBool   Bool
    | LitChar   Char
    | LitString String
   -- | LitRange  Range
    -- Operators
    | ExpBinary Binary Expression Expression DataType
    | ExpUnary  Unary  Expression            DataType
    | ExpError  DataType
--    | ExpressionArry ExpressionArry
    deriving (Show, Typeable)



dataType :: Expression -> DataType
dataType (Variable _)        = undefined
dataType (LitInt _)          = Int
dataType (LitFloat _)        = Float
dataType (LitBool _)         = Bool
dataType (LitChar _)         = Char
dataType (LitString _)       = String
dataType (ExpBinary _ _ _ d) = d
dataType (ExpUnary _ _ d)    = d
dataType (ExpError a)        = a























type Checker a = ErrorT LexError (RWST CheckReader CheckWriter CheckState Identity) a

data Flag = Flag1 | Flag2 | Flag3
type CheckReader = [Flag]

data LexError = UnexpectedChar String deriving (Show)
data ParseError = UnexpectedToken String deriving (Show)
data StaticError
    = BinaryTypes Binary [DataType]
    | UnaryTypes  Unary DataType
    | StaticError String

instance Show StaticError where
    show (UnaryTypes op dt)   = "Static Error: operator " ++ show op ++ "doesn't work with arguments " ++ show dt
    show (BinaryTypes op dts) = "Static Error: operator " ++ show op ++ " doesn't work with arguments " ++  take 2 (concatMap (\dt -> ", " ++ show dt) dts)
    show (StaticError str)    = str
type CheckWriter = [Either ParseError StaticError]

data CheckState = CheckState
    { symtable :: ()
    , whatevs  :: ()
    }
    deriving (Show)

instance Error LexError where
    noMsg  = UnexpectedChar "Lexical error"
    strMsg = UnexpectedChar

flags :: CheckReader
flags = []

initialState :: CheckState
initialState = CheckState () ()



runChecker :: Checker a -> (Either LexError a, CheckState, CheckWriter)
runChecker = runIdentity . flip (`runRWST` flags) initialState . runErrorT

getWriter :: Checker a -> CheckWriter
getWriter = (\(_,_,w) -> w) . runChecker

getCheck :: Checker a -> Either LexError a
getCheck = (\(c,_,_) -> c) . runChecker

getState :: Checker a -> CheckState
getState = (\(_,s,_) -> s) . runChecker








-------------------------------------------------------------------------------

--class Printer p where
--    treePrint :: Int -> p -> String

--rep :: Int -> String
--rep n = replicate n '\t'

--instance (Printer a, Typeable a, Show a) => Printer [a] where
--    treePrint n ls
--        | ((show $ typeOf ls) == "[Char]") = rep n ++ show ls ++ "\n"
--        | otherwise = concat (map (treePrint n) ls)

--instance Printer Char where
--    treePrint n = show

--instance Printer Int where
--    treePrint n i = rep n ++ show i ++ "\n"

--instance Printer Float where
--    treePrint n f = rep n ++ show f ++ "\n"

--instance Printer Bool where
--    treePrint n b = rep n ++ show b ++ "\n"

--instance Printer DataType where
--    treePrint n k = rep n ++ show k ++ "\n"

--instance Printer Declaration where
--    treePrint n (Declaration var typ) = rep n ++ "Declaration" ++ "\n" ++ treePrint (n+1) var ++ treePrint (n+1) typ ++ "\n"

--instance Printer Statement where
--    treePrint n StNoop = rep n ++ "StNoop" ++ "\n"
--    treePrint n (StAssign var expr)           = rep n ++ "StAssign" ++ "\n" ++ treePrint (n+1) var ++ treePrint (n+1) expr ++ "\n"
--    treePrint n (StDeclaration decls)         = rep n ++ "StDeclaration" ++ "\n" ++ treePrint (n+1) decls ++ "\n"
--    treePrint n (StReturn expr)               = rep n ++ "StReturn" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
--    treePrint n (StRead   vars)               = rep n ++ "StRead"   ++ "\n" ++ treePrint (n+1) vars ++ "\n"
--    treePrint n (StPrint  exprs)              = rep n ++ "StPrint"  ++ "\n" ++ treePrint (n+1) exprs ++ "\n"
--    treePrint n (StIf     bool ifSts elseSts) = rep n ++ "StIf"     ++ "\n" ++ treePrint (n+1) bool ++ rep n ++ "then\n" ++ treePrint (n+1) ifSts ++ rep n ++ "else\n" ++ treePrint (n+1) elseSts ++ "\n"
--    treePrint n (StCase   arit cases elseSts) = rep n ++ "StCase"   ++ "\n" ++ treePrint (n+1) arit ++ treePrint (n+1) cases ++ treePrint (n+1) elseSts  ++ "\n"
--    treePrint n (StWhile  bool sts)           = rep n ++ "StWhile"  ++ "\n" ++ treePrint (n+1) bool ++ treePrint (n+1) sts ++ "\n"
--    treePrint n (StFor    var rang sts)       = rep n ++ "StFor"    ++ "\n" ++ treePrint (n+1) var  ++ treePrint (n+1) rang  ++ treePrint (n+1) sts  ++ "\n"
--    treePrint n StBreak    = rep n ++ "StBreak" ++ "\n"
--    treePrint n StContinue = rep n ++ "StContinue" ++ "\n"

--instance Printer Case where
--    treePrint n (Case expr sts) = rep n ++ "Case" ++ "\n" ++ treePrint (n+1) expr ++ treePrint (n+1) sts ++ "\n"

--instance Printer Expression where
--    treePrint n (ExpressionId var)    = rep n ++ "ExpressionId"   ++ "\n" ++ treePrint (n+1) var ++ "\n"
--    treePrint n (ExpressionArit expr) = rep n ++ "ExpressionArit" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
--    treePrint n (ExpressionBool expr) = rep n ++ "ExpressionBool" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
--    treePrint n (ExpressionStrn expr) = rep n ++ "ExpressionStrn" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
--    treePrint n (ExpressionRang expr) = rep n ++ "ExpressionRang" ++ "\n" ++ treePrint (n+1) expr ++ "\n"

--instance Printer ExpressionArit where
--    treePrint n (LiteralInt num)   = rep n ++ "LiteralInt"      ++ "\n" ++ treePrint (n+1) num ++ "\n"
--    treePrint n (LiteralFloat num) = rep n ++ "LiteralFloat"    ++ "\n" ++ treePrint (n+1) num ++ "\n"
--    treePrint n (PlusArit   left right) = rep n ++ "PlusArit"   ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (MinusArit  left right) = rep n ++ "MinusArit"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (TimesArit  left right) = rep n ++ "TimesArit"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (DivideArit left right) = rep n ++ "DivideArit" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (ModuloArit left right) = rep n ++ "ModuloArit" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (PowerArit  left right) = rep n ++ "PowerArit"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (NegateArit expr) = rep n ++ "NegateArit"       ++ "\n" ++ treePrint (n+1) expr ++ "\n"

--instance Printer ExpressionBool where
--    treePrint n (LiteralBool bool) = rep n ++ "LiteralBool" ++ "\n" ++ treePrint (n+1) bool ++ "\n"
--    treePrint n (OrBool      left right) = rep n ++ "OrBool"      ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (AndBool     left right) = rep n ++ "AndBool"     ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (NotBool     expr) = rep n ++ "NotBool" ++ "\n" ++ treePrint (n+1) expr ++ "\n"
--    treePrint n (EqualBool   left right) = rep n ++ "EqualBool"   ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (UnequalBool left right) = rep n ++ "UnequalBool" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (LessBool    left right) = rep n ++ "LessBool"    ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (LessEqBool  left right) = rep n ++ "LessEqBool"  ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (GreatBool   left right) = rep n ++ "GreatBool"   ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (GreatEqBool left right) = rep n ++ "GreatEqBool" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"
--    treePrint n (BelongsBool left right) = rep n ++ "BelongsBool" ++ "\n" ++ treePrint (n+1) left ++ treePrint (n+1) right ++ "\n"

--instance Printer ExpressionStrn where
--    treePrint n (LiteralStrn s) = rep n ++ "LiteralStrn" ++ "\n" ++ treePrint (n+1) s ++ "\n"

--instance Printer ExpressionRang where
--    treePrint n (LiteralRang from to) = rep n ++ "LiteralRang" ++ "\n" ++ treePrint (n+1) from ++ treePrint (n+1) to ++ "\n"


