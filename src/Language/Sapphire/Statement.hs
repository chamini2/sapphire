module Language.Sapphire.Statement
    ( Statement(..)
    , StBlock
    , When(..)
    , Signature(..)
    ) where

import           Language.Sapphire.DataType
import           Language.Sapphire.Declaration
import           Language.Sapphire.Expression
import           Language.Sapphire.Identifier
import           Language.Sapphire.Lexeme

import           Data.Foldable                 (foldl')
import           Data.Sequence                 (Seq)
import           Data.List                     (intercalate)

type StBlock    = Seq (Lexeme Statement)

----------------------------------------

data Statement
    -- Language
    = StNoop                                                -- Only used in Parser
    | StAssign (Lexeme Access) (Lexeme Expression)
    -- Definitions
    | StVariableDeclaration (Lexeme Declaration)
    | StDeclarationList     (Seq (Lexeme Declaration))      -- Only used in Parser
    | StStructDefinition    (Lexeme DataType) (Seq Field)
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

instance Show Signature where
    show (Sign parmLs retDtL) = showParms ++ showRetDt
        where
            showParms = intercalate ", " $ foldl' func [] parmLs
            showRetDt = " -> " ++ show (lexInfo retDtL)
            func ls (Lex dcl _) = (show (lexInfo $ dclDataType dcl) ++ " " ++ lexInfo (dclIdentifier dcl)) : ls
