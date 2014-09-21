{-|
    Three-address code (TAC) generation module
-}
module TAC (
    TACGenerator
) 
where

import           Data.Sequence             as DS (Seq, empty, length)

{-|
    Three-address code representation
-}

data Variable = 

type TACtemporary = String

type Label = String

data Instruction = 
      Assign Operator Arg1 Arg2 Result
    -- Function related instructions
    | BeginFunction <Number of bytes>
    | EndFunction
    | PushParameter       
    | PopParameters
    | Return
    | LCall
    | ACall
    -- GoTo
    | GoTo TACLabel
    | IfZ TACTemporary TACLabel
    -- Move
    | Move _ _
    -- Store 
    | Store _ _ _
    -- Load
    | Load _ _ _

data Binary = Add | Sub | Mul | Div | Mod
    deriving (Show, Eq)

data Unary = undefined

{-|
    TAC Generator Monad 

    This monad needs to generate the necessary intermediate representation (IR) 
    in this case the three-address code; store it temporarily as a structure for
    further processing and send it to a temporary file for debugging purposes.
-}

type TACGenerator = RWS TACReader TACWriter TACState

-- State

TACState :: TACState
TACState = TACState
    { tempSerial  :: Int
    , labelSerial :: Int
    -- Symbol Table
    } deriving 

instance Show TACState where
    show (TACState _ _) = undefined

initialState :: TACState
initialState = TACState 
    { tempSerial  = 0
    , labelSerial = 0
    }

-- Writer

type TACWriter = Seq TACInstruction

newTemporary :: TACGenerator TACTemporary
newTemporary = modify return tempSerial 

generate

StatementToTAC :: Statement -> TACGenerator ()
StatementToTAC st = case st of

ExpressionToTAC :: Statement -> TacGenerator ()
ExpressionToTAC e = case e of
