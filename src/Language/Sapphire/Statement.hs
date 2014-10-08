{-# LANGUAGE LambdaCase #-}
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

import           Data.Foldable                 (foldl', concatMap)
import           Data.Sequence                 (Seq)
import           Data.List                     (intercalate)
import           Prelude                       hiding (exp, concatMap)

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
    | StCase (Lexeme Expression) (Seq (Lexeme When))     StBlock
    -- Loops
    | StLoop     StBlock (Lexeme Expression) StBlock
    | StFor      (Lexeme Identifier) (Lexeme Expression) StBlock
    | StBreak
    | StContinue

instance Show Statement where
    show = \case
        StAssign accL expL         -> show (lexInfo accL) ++ " = " ++ show (lexInfo expL)
        StVariableDeclaration dclL -> show (lexInfo dclL)
        StStructDefinition dtL _   -> show (lexInfo dtL)
        StReturn expL              -> "return " ++ show (lexInfo expL)
        StFunctionDef idnL _ _     -> "def " ++ lexInfo idnL
        StProcedureCall idnL expLs -> lexInfo idnL ++ "(" ++ concatMap (show . lexInfo) expLs ++ ")"
        StRead accL                -> "read " ++ show (lexInfo accL)
        StPrint expL               -> "print " ++ show (lexInfo expL)
        StIf expL _ _              -> "if " ++ show (lexInfo expL)
        StCase expL _ _            -> "case " ++ show (lexInfo expL)
        StLoop _ expL _            -> "repeat .. while " ++ show (lexInfo expL) ++ "do .. end"
        StFor idnL expL _          -> "for " ++ lexInfo idnL ++ " in " ++ show (lexInfo expL) ++ " do "
        StBreak                    -> "break"
        StContinue                 -> "continue"
        _                          -> error "Statement.Statement.show: should not show these statements"
        -- StNoop
        -- StDeclarationList
        -- StReadString
        -- StPrintList

----------------------------------------

data When = When (Seq (Lexeme Expression)) StBlock

data Signature = Sign (Seq (Lexeme Declaration)) (Lexeme DataType)

instance Show Signature where
    show (Sign prmLs retDtL) = showPrms ++ showRetDt
        where
            showPrms = intercalate ", " $ foldl' func [] prmLs
            showRetDt = " -> " ++ show (lexInfo retDtL)
            func ls (Lex dcl _) = (show (lexInfo $ dclDataType dcl) ++ " " ++ lexInfo (dclIdentifier dcl)) : ls
