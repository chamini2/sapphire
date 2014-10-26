{-# LANGUAGE LambdaCase #-}
module Language.Sapphire.Program
    ( Program(..)

    , module Language.Sapphire.DataType
    , module Language.Sapphire.Declaration
    , module Language.Sapphire.Expression
    , module Language.Sapphire.Identifier
    , module Language.Sapphire.Lexeme
    , module Language.Sapphire.Statement
    ) where

import           Language.Sapphire.DataType
import           Language.Sapphire.Declaration
import           Language.Sapphire.Expression
import           Language.Sapphire.Identifier
import           Language.Sapphire.Lexeme
import           Language.Sapphire.Statement

import           Control.Monad.State       (StateT, get, modify, runStateT)
import           Control.Monad.Writer      (Writer, execWriter, tell)
import           Data.Char                 (toLower)
import           Data.Foldable             (concat, forM_, mapM_)
import           Data.Sequence             (Seq, singleton)
import           Prelude                   hiding (concat, exp, mapM_)

--------------------------------------------------------------------------------

newtype Program = Program StBlock

instance Show Program where
    show = processPrettyShow

--------------------------------------------------------------------------------

type PrettyShow a = StateT Tabs (Writer (Seq String)) a

--------------------------------------------------------------------------------
-- State

type Tabs = Int

----------------------------------------
-- Initial

initialState :: Tabs
initialState =  0

----------------------------------------
-- Extra

tabs :: Int -> String
tabs n = replicate n '\t'

--------------------------------------------------------------------------------
-- Building the Monad

buildPrettyShow :: Program -> PrettyShow ()
buildPrettyShow (Program block) = prettyStatements "PROGRAM" block

--------------------------------------------------------------------------------
-- Using the Monad

processPrettyShow :: Program -> String
processPrettyShow = runPrettyShow . buildPrettyShow

runPrettyShow :: PrettyShow () -> String
runPrettyShow = concat . execWriter . flip runStateT initialState

--------------------------------------------------------------------------------
-- Monad handling

prettyString :: String -> PrettyShow ()
prettyString str = get >>= \t -> tell . singleton $ tabs t ++ str ++ "\n"

----------------------------------------
-- Tabs

raiseTabs :: PrettyShow ()
raiseTabs = modify succ

lowerTabs :: PrettyShow ()
lowerTabs = modify pred

--------------------------------------------------------------------------------

prettyStatements :: String -> StBlock -> PrettyShow ()
prettyStatements str block = do
    prettyString str

    raiseTabs
    mapM_ prettyStatement block
    lowerTabs

prettyStatement :: Lexeme Statement -> PrettyShow ()
prettyStatement (Lex st posn) = case st of

    StAssign accL expL -> do
        prettyString $ "ASSIGNMENT " ++ show posn ++ ":"
        raiseTabs
        prettyString $ "- variable: " ++ show (lexInfo accL)
        prettyExpressionWithTag "- value: " (lexInfo expL)
        lowerTabs

    StVariableDeclaration dclL -> do
        prettyString $ "DECLARATION " ++ show posn ++ ":"
        raiseTabs
        prettyString $ "- variable: " ++ (lexInfo . dclIdentifier $ lexInfo dclL)
        prettyString $ "- data type: " ++ (show . lexInfo . dclDataType $ lexInfo dclL)
        lowerTabs

    StStructDefinition dtL flds -> do
        let strct = case lexInfo dtL of
                Record _ -> "RECORD"
                Union  _ -> "UNION"
                _        -> error "Program.prettyStatement: StStructDefinition has non-struct DataType"
        prettyString $ strct ++ " DEFINITION " ++ show posn ++ ":"
        raiseTabs
        forM_ flds $ \(Lex fldIdn _, Lex fldDt _) ->
            prettyString $ "- field: " ++ fldIdn ++ " : " ++ show fldDt
        lowerTabs

    StReturn expL -> do
        prettyString $ "RETURN " ++ show posn ++ ":"
        raiseTabs
        prettyExpression (lexInfo expL)
        lowerTabs

    StFunctionDef idnL sign block -> do
        prettyString $ "FUNCTION DEFINITION " ++ show posn ++ ":"
        raiseTabs

        prettyString $ "- function: " ++ lexInfo idnL
        prettyString "- signature: "
        raiseTabs
        prettyString $ show sign
        lowerTabs

        prettyStatements "- body:" block

        lowerTabs

    StProcedureCall idnL args -> do
        prettyString $ "PROCEDURE CALL " ++ show posn ++ ":"
        raiseTabs

        prettyString $ "- function: " ++ lexInfo idnL
        prettyString "- arguments: "
        raiseTabs
        mapM_ (prettyExpression . lexInfo) args
        lowerTabs

        lowerTabs

    StRead accL -> do
        prettyString $ "READ " ++ show posn ++ ":"
        raiseTabs
        prettyString $ "- variable: " ++ show (lexInfo accL)
        lowerTabs

    StPrint expL -> do
        prettyString $ "PRINT " ++ show posn ++ ":"
        raiseTabs
        prettyExpressionWithTag "- expression: " (lexInfo expL)
        lowerTabs

    StIf expL trueBlock falseBlock -> do
        prettyString $ "IF " ++ show posn ++ ":"
        raiseTabs
        prettyExpressionWithTag "- condition:" (lexInfo expL)
        prettyStatements "- then:" trueBlock
        prettyStatements "- else:" falseBlock
        lowerTabs

    StCase expL whnLs othrBlock -> do
        prettyString $ "CASE " ++ show posn ++ ":"
        raiseTabs
        prettyExpressionWithTag "- expression: " (lexInfo expL)

        forM_ whnLs $ \(Lex (When whnExpLs whnBlock) _) -> do
            prettyString "- when:"
            raiseTabs
            mapM_ (prettyExpression . lexInfo) whnExpLs
            lowerTabs
            prettyStatements "- do:" whnBlock

        prettyStatements "- otherwise:" othrBlock
        lowerTabs

    StLoop befBLock expL aftBlock -> do
        prettyString $ "LOOP " ++ show posn ++ ":"
        raiseTabs
        prettyStatements "- repeat:" befBLock
        prettyExpressionWithTag "- condition:" (lexInfo expL)
        prettyStatements "- while:" aftBlock
        lowerTabs

    StFor idnL expL block -> do
        prettyString $ "FOR " ++ show posn ++ ":"
        raiseTabs
        prettyString $ "- variable: " ++ lexInfo idnL
        prettyExpressionWithTag "- range:" (lexInfo expL)
        prettyStatements "- body" block
        lowerTabs


    StBreak    -> prettyString $ "BREAK"    ++ show posn
    StContinue -> prettyString $ "CONTINUE" ++ show posn

    _ -> error $ "Program.prettyStatement: statement '" ++ show st ++ "' should not shown in the AST"

prettyExpression :: Expression -> PrettyShow ()
prettyExpression = \case

    LitInt    i -> prettyString $ "Int literal: "   ++ show (lexInfo i)
    LitChar   c -> prettyString $ "Char literal: " ++ show (lexInfo c)
    LitBool   b -> prettyString $ "Bool literal: "   ++ map toLower (show (lexInfo b))
    LitFloat  f -> prettyString $ "Float literal: "     ++ show (lexInfo f)
    LitString s -> prettyString $ "String literal: "    ++ show (lexInfo s)

    Variable accL -> prettyString $ "Access: " ++ show (lexInfo accL)

    FunctionCall iden args -> do
        prettyString "FUNCTION CALL:"
        raiseTabs

        prettyString $ "- function: " ++ show (lexInfo iden)
        prettyString "- arguments: "
        raiseTabs
        mapM_ (prettyExpression . lexInfo) args
        lowerTabs

        lowerTabs

    ExpBinary opL lExpL rExpL -> do
        prettyString "BINARY OPERATION:"
        raiseTabs
        prettyString $ "- operator: " ++ show (lexInfo opL)
        prettyExpressionWithTag "- left operand:  " (lexInfo lExpL)
        prettyExpressionWithTag "- right operand: " (lexInfo rExpL)
        lowerTabs

    ExpUnary opL expL -> do
        prettyString "UNARY OPERATION:"
        raiseTabs
        prettyString $ "- operator: " ++ show (lexInfo opL)
        prettyExpressionWithTag "- operand: " (lexInfo expL)
        lowerTabs

prettyExpressionWithTag :: String -> Expression -> PrettyShow ()
prettyExpressionWithTag tag exp = do
    prettyString tag
    raiseTabs
    prettyExpression exp
    lowerTabs
