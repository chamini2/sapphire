module Statement
    ( Statement(..)
    , StBlock
    , When(..)
    , Signature(..)
    ) where

import           DataType
import           Declaration
import           Expression
import           Identifier
import           Lexeme

import           Data.Sequence (Seq)

type StBlock    = Seq (Lexeme Statement)

----------------------------------------

data Statement
    -- Language
    = StNoop                                                -- Only used in Parser
    | StAssign (Lexeme Access) (Lexeme Expression)
    -- Definitions
    | StVariableDeclaration (Lexeme Declaration)
    | StDeclarationList     (Seq (Lexeme Declaration))      -- Only used in Parser
    | StStructDefinition    (Lexeme DataType)
    -- Functions
    | StReturn        (Lexeme Expression)
    | StFunctionDef   (Lexeme Identifier) Signature StBlock
    | StProcedureCall (Lexeme Identifier) (Seq (Lexeme Expression))
    -- I/O
    | StRead       (Lexeme Access)
    | StReadString (Maybe (Lexeme String)) (Lexeme Access)  -- Only used in Parser
    | StPrint      (Lexeme Expression)
    | StPrintList  (Seq (Lexeme Expression))                -- Only used in Parser
    -- Conditional
    | StIf   (Lexeme Expression) StBlock StBlock
    | StCase (Lexeme Expression) (Seq (Lexeme When))      StBlock
    -- Loops
    | StLoop     StBlock (Lexeme Expression) StBlock
    | StFor      (Lexeme Identifier) (Lexeme Expression)  StBlock
    | StBreak
    | StContinue
    deriving (Show)

--instance Show Statement where
--    show = runPrinter . printStatement

----------------------------------------

data When = When (Seq (Lexeme Expression)) StBlock
    deriving (Show)


data Signature = Sign (Seq (Lexeme Declaration)) (Lexeme DataType)
    deriving (Show)
