{-# LANGUAGE LambdaCase #-}
module Language.Sapphire.Printer
    ( processPrinter
    ) where

import           Language.Sapphire.Program

import           Control.Monad.State       (StateT, get, modify, runStateT)
import           Control.Monad.Writer      (Writer, execWriter, tell)
import           Data.Char                 (toLower)
import           Data.Foldable             (concat, forM_, mapM_)
import           Data.Sequence             (Seq, singleton)
import           Prelude                   hiding (concat, exp, mapM_)

--------------------------------------------------------------------------------

instance Show Program where
    show = processPrinter

--------------------------------------------------------------------------------

type Printer a = StateT Tabs (Writer (Seq String)) a

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

buildPrinter :: Program -> Printer ()
buildPrinter (Program block) = printStatements "PROGRAM" block

--------------------------------------------------------------------------------
-- Using the Monad

processPrinter :: Program -> String
processPrinter = runPrinter . buildPrinter

runPrinter :: Printer () -> String
runPrinter = concat . execWriter . flip runStateT initialState

--------------------------------------------------------------------------------
-- Monad handling

printString :: String -> Printer ()
printString str = get >>= \t -> tell . singleton $ tabs t ++ str ++ "\n"

----------------------------------------
-- Tabs

raiseTabs :: Printer ()
raiseTabs = modify succ

lowerTabs :: Printer ()
lowerTabs = modify pred

--------------------------------------------------------------------------------

printStatements :: String -> StBlock -> Printer ()
printStatements str block = do
    printString str

    raiseTabs
    mapM_ printStatement block
    lowerTabs

printStatement :: Lexeme Statement -> Printer ()
printStatement (Lex st posn) = case st of

    StAssign accL expL -> do
        printString $ "ASSIGNMENT " ++ show posn ++ ":"
        raiseTabs
        printString $ "- variable: " ++ show (lexInfo accL)
        printExpressionWithTag "- value: " (lexInfo expL)
        lowerTabs

    StVariableDeclaration dclL -> do
        printString $ "DECLARATION " ++ show posn ++ ":"
        raiseTabs
        printString $ "- variable: " ++ (lexInfo . dclIdentifier $ lexInfo dclL)
        printString $ "- data type: " ++ (show . lexInfo . dclDataType $ lexInfo dclL)
        lowerTabs

    StStructDefinition dtL flds -> do
        let strct = case lexInfo dtL of
                Record _ -> "RECORD"
                Union  _ -> "UNION"
        printString $ strct ++ " DEFINITION " ++ show posn ++ ":"
        raiseTabs
        forM_ flds $ \(Lex fldIdn _, Lex fldDt _) ->
            printString $ "- field: " ++ fldIdn ++ " : " ++ show fldDt
        lowerTabs

    StReturn expL -> do
        printString $ "RETURN " ++ show posn ++ ":"
        raiseTabs
        printExpression (lexInfo expL)
        lowerTabs

    StFunctionDef idnL sign block -> do
        printString $ "FUNCTION DEFINITION " ++ show posn ++ ":"
        raiseTabs

        printString $ "- function: " ++ lexInfo idnL
        printString "- signature: "
        raiseTabs
        printString $ show sign
        lowerTabs

        printStatements "- body:" block

        lowerTabs

    StProcedureCall idnL args -> do
        printString $ "PROCEDURE CALL " ++ show posn ++ ":"
        raiseTabs

        printString $ "- function: " ++ lexInfo idnL
        printString "- arguments: "
        raiseTabs
        mapM_ (printExpression . lexInfo) args
        lowerTabs

        lowerTabs

    StRead accL -> do
        printString $ "READ " ++ show posn ++ ":"
        raiseTabs
        printString $ "- variable: " ++ show (lexInfo accL)
        lowerTabs

    StPrint expL -> do
        printString $ "PRINT " ++ show posn ++ ":"
        raiseTabs
        printExpressionWithTag "- expression: " (lexInfo expL)
        lowerTabs

    StIf expL trueBlock falseBlock -> do
        printString $ "IF " ++ show posn ++ ":"
        raiseTabs
        printExpressionWithTag "- condition:" (lexInfo expL)
        printStatements "- then:" trueBlock
        printStatements "- else:" falseBlock
        lowerTabs

    StCase expL whnLs othrBlock -> do
        printString $ "CASE " ++ show posn ++ ":"
        raiseTabs
        printExpressionWithTag "- expression: " (lexInfo expL)

        forM_ whnLs $ \(Lex (When whnExpLs whnBlock) _) -> do
            printString "- when:"
            raiseTabs
            mapM_ (printExpression . lexInfo) whnExpLs
            lowerTabs
            printStatements "- do:" whnBlock

        printStatements "- otherwise:" othrBlock
        lowerTabs

    StLoop befBLock expL aftBlock -> do
        printString $ "LOOP " ++ show posn ++ ":"
        raiseTabs
        printStatements "- repeat:" befBLock
        printExpressionWithTag "- condition:" (lexInfo expL)
        printStatements "- while:" aftBlock
        lowerTabs

    StFor idnL expL block -> do
        printString $ "FOR " ++ show posn ++ ":"
        raiseTabs
        printString $ "- variable: " ++ lexInfo idnL
        printExpressionWithTag "- range:" (lexInfo expL)
        printStatements "- body" block
        lowerTabs


    StBreak    -> printString $ "BREAK"    ++ show posn
    StContinue -> printString $ "CONTINUE" ++ show posn

    _ -> error $ "Printer.printStatement: statement '" ++ show st ++ "' should not shown in the AST"

printExpression :: Expression -> Printer ()
printExpression = \case

    LitInt    i -> printString $ "Int literal: "   ++ show (lexInfo i)
    LitChar   c -> printString $ "Char literal: " ++ show (lexInfo c)
    LitBool   b -> printString $ "Bool literal: "   ++ map toLower (show (lexInfo b))
    LitFloat  f -> printString $ "Float literal: "     ++ show (lexInfo f)
    LitString s -> printString $ "String literal: "    ++ show (lexInfo s)

    Variable accL -> printString $ "Access: " ++ show (lexInfo accL)

    FunctionCall iden args -> do
        printString "FUNCTION CALL:"
        raiseTabs

        printString $ "- function: " ++ show (lexInfo iden)
        printString "- arguments: "
        raiseTabs
        mapM_ (printExpression . lexInfo) args
        lowerTabs

        lowerTabs

    ExpBinary opL lExpL rExpL -> do
        printString "BINARY OPERATION:"
        raiseTabs
        printString $ "- operator: " ++ show (lexInfo opL)
        printExpressionWithTag "- left operand:  " (lexInfo lExpL)
        printExpressionWithTag "- right operand: " (lexInfo rExpL)
        lowerTabs

    ExpUnary opL expL -> do
        printString "UNARY OPERATION:"
        raiseTabs
        printString $ "- operator: " ++ show (lexInfo opL)
        printExpressionWithTag "- operand: " (lexInfo expL)
        lowerTabs

printExpressionWithTag :: String -> Expression -> Printer ()
printExpressionWithTag tag exp = do
    printString tag
    raiseTabs
    printExpression exp
    lowerTabs
