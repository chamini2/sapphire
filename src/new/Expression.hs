module Expression
    ( Expression(..)
    , Binary(..)
    , Unary(..)
    , binaryOperation
    , unaryOperation

    , Access(..)
    , AccessHistory(..)

    , Thread
    , Zipper
    , focusAccess
    , defocusAccess
    , inArrayAccess
    , inStructAccess
    , inAccess
    , backAccess
    , topAccess
    , deepAccess
    ) where

import           DataType
import           Identifier
import           Lexeme

--import qualified Data.Data     as DD (Data)
import           Data.Functor  ((<$))
import           Data.Maybe    (fromJust)
import           Data.Sequence as DS (Seq, fromList)
--import qualified Data.Typeable as DT (Typeable)

data Expression
    -- Variable
    = Variable (Lexeme Access)
    -- Function call
    | FunctionCall (Lexeme Identifier) (Seq (Lexeme Expression))
    -- Literals
    | LitInt    (Lexeme Int)
    | LitFloat  (Lexeme Float)
    | LitBool   (Lexeme Bool)
    | LitChar   (Lexeme Char)
    | LitString (Lexeme String)
--    | LitRange  (Lexeme Range)
    -- Operators
    | ExpBinary (Lexeme Binary) (Lexeme Expression) (Lexeme Expression)
    | ExpUnary  (Lexeme Unary)  (Lexeme Expression)
    deriving (Eq, Ord, Show)

--instance Show Expression where
--    show = runPrinter . printExpression

data Binary
    = OpPlus  | OpMinus   | OpTimes | OpDivide | OpModulo | OpPower   | OpFromTo
    | OpEqual | OpUnequal | OpLess  | OpLessEq | OpGreat  | OpGreatEq
    | OpBelongs
    | OpOr    | OpAnd
    deriving (Eq, Ord)

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
    deriving (Eq, Ord)

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

--------------------------------------------------------------------------------

data Access = VariableAccess (Lexeme Identifier)
            | ArrayAccess    (Lexeme Access)     (Lexeme Expression)
            | StructAccess   (Lexeme Access)     (Lexeme Identifier)
            deriving (Eq, Ord)

instance Show Access where
    show acc = case acc of
        VariableAccess idnL      -> lexInfo idnL
        ArrayAccess    accL indL -> show (lexInfo accL) ++ "[" ++ show (lexInfo indL) ++ "]"
        StructAccess   accL fldL -> show (lexInfo accL) ++ "." ++ lexInfo fldL

{-
 - deriving the AccessHistory data
 -
 - Access        = acc
 - AccessHistory = acc'
 -
 - acc = (var * identifier) + (arr * acc * expression) + (str * acc * identifier)
 - expression = A
 - identifier = B
 -
 - acc = (1 * B) + (1 * acc * A) + (1 * acc * B)
 - acc = B       + (acc * A)     + (acc * B)
 -
 - acc' = A + B
 -}

data AccessHistory = HistoryArray  (Lexeme Expression)
                   | HistoryStruct (Lexeme Identifier)
                   deriving (Show)

type Thread = [Lexeme AccessHistory]

type Zipper = (Lexeme Access, Thread)

----------------------------------------

focusAccess :: Lexeme Access -> Zipper
focusAccess accL = (accL, [])

defocusAccess :: Zipper -> Lexeme Access
defocusAccess (accL, _) = accL

inArrayAccess :: Zipper -> Maybe Zipper
inArrayAccess (histL@(Lex acc _), thrd) = case acc of
    ArrayAccess accL indexL -> Just (accL, (HistoryArray indexL <$ histL) : thrd)
    _                       -> Nothing

inStructAccess :: Zipper -> Maybe Zipper
inStructAccess (histL@(Lex acc _), thrd) = case acc of
    StructAccess accL fieldL -> Just (accL, (HistoryStruct fieldL <$ histL) : thrd)
    _                        -> Nothing

inAccess :: Zipper -> Maybe Zipper
inAccess zpp@(accL,_) = case lexInfo accL of
    VariableAccess _   -> Nothing
    ArrayAccess    _ _ -> inArrayAccess zpp
    StructAccess   _ _ -> inStructAccess zpp

backAccess :: Zipper -> Maybe Zipper
backAccess (accL, thrd) = case thrd of
    []                      -> Nothing
    hstL@(Lex hist _) : hstLs -> case hist of
        HistoryArray  indexL -> Just (ArrayAccess  accL indexL <$ hstL, hstLs)
        HistoryStruct fieldL -> Just (StructAccess accL fieldL <$ hstL, hstLs)

topAccess :: Zipper -> Zipper
topAccess zpp = case snd zpp of
    []     -> zpp
    _ : _ -> topAccess $ fromJust $ backAccess zpp

deepAccess :: Zipper -> Zipper
deepAccess zpp@(accL, _) = case lexInfo accL of
    VariableAccess _ -> zpp
    _                -> deepAccess $ fromJust $ inAccess zpp
