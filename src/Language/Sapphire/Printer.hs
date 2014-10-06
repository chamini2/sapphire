module Language.Sapphire.Printer
    ( processPrinter
    )where

import           Language.Sapphire.Program

import           Control.Monad.State  (StateT, runStateT, modify, get)
import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Char            (toLower)
import           Data.Sequence        (Seq, singleton)
import           Data.Foldable        (concat, mapM_, forM_)
import           Prelude              hiding (concat, mapM_, exp)

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
buildPrinter (Program block) = do
    printString "Program"
    raiseTabs >> printStatements block >> lowerTabs

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

printStatements :: StBlock -> Printer ()
printStatements = mapM_ printStatement

printStatement :: Lexeme Statement -> Printer ()
printStatement (Lex st posn) = case st of

    StAssign accL expL -> do
        printString $ "ASSIGNMENT " ++ show posn ++ ":"
        raiseTabs >> do
            printString $ "- variable: " ++ show (lexInfo accL)
            printExpressionWithTag "- value: " (lexInfo expL)
        lowerTabs

    StVariableDeclaration dclL -> do
        printString $ "DECLARATION " ++ show posn ++ ":"
        raiseTabs >> do
            printString $ "- variable: " ++ (lexInfo . dclIdentifier $ lexInfo dclL)
            printString $ "- data type: " ++ (toIdentifier . lexInfo . dclDataType $ lexInfo dclL)
        lowerTabs

    StStructDefinition dtL flds -> do
        let strct = case lexInfo dtL of
                Record _ -> "RECORD"
                Union  _ -> "UNION"
        printString $ strct ++ " DEFINITION " ++ show posn ++ ":"
        raiseTabs >> do
            forM_ flds $ \(Lex fldIdn _, Lex fldDt _) ->
                printString $ "- field: " ++ fldIdn ++ " : " ++ show fldDt
        lowerTabs

    StReturn expL -> do
        printString $ "RETURN " ++ show posn ++ ":"
        raiseTabs >>
            printExpression (lexInfo expL)
        lowerTabs

    StFunctionDef idnL sign block -> do
        printString $ "FUNCTION DEFINITION " ++ show posn ++ ":"
        raiseTabs >> do
            printString $ "- function: " ++ lexInfo idnL

            printString "- signature: "
            raiseTabs >> do
                printString $ show sign
            lowerTabs

            printString "- body:"
            raiseTabs >> do
                printStatements block
            lowerTabs

        lowerTabs

    StProcedureCall idnL args -> do
        printString $ "PROCEDURE CALL " ++ show posn ++ ":"
        raiseTabs >> do
            printString $ "- function: " ++ lexInfo idnL

            printString "- arguments: "

            raiseTabs >>
                mapM_ (printExpression . lexInfo) args
            lowerTabs

        lowerTabs

    StRead accL -> do
        printString $ "READ " ++ show posn ++ ":"
        raiseTabs >> do
            printString $ "- variable: " ++ show (lexInfo accL)
        lowerTabs

    StPrint expL -> do
        printString $ "PRINT " ++ show posn ++ ":"
        raiseTabs >> do
            printExpressionWithTag "- expression: " (lexInfo expL)
        lowerTabs

    StIf expL trueBlock falseBlock -> do
        printString $ "IF " ++ show posn ++ ":"
        raiseTabs >> do
            printExpressionWithTag "- condition:" (lexInfo expL)

            printString "- then:"
            raiseTabs >> do
                printStatements trueBlock
            lowerTabs

            printString "- else:"
            raiseTabs >> do
                printStatements falseBlock
            lowerTabs
        lowerTabs

    StCase expL whnLs othrBlock -> do
        printString $ "CASE " ++ show posn ++ ":"
        raiseTabs >> do
            printExpressionWithTag "- expression: " (lexInfo expL)

            forM_ whnLs $ \(Lex (When whnExpLs whnBlock) _) -> do
                printString "- when:"
                raiseTabs >> do
                    mapM_ (printExpression . lexInfo) whnExpLs
                lowerTabs

                printString "- do:"
                raiseTabs >> do
                    printStatements whnBlock
                lowerTabs

            printString "- otherwise:"
            raiseTabs >> do
                printStatements othrBlock
            lowerTabs
        lowerTabs

    StLoop befBLock expL aftBlock -> do
        printString $ "LOOP " ++ show posn ++ ":"
        raiseTabs >> do
            printString "- repeat:"
            raiseTabs >> do
                printStatements befBLock
            lowerTabs

            printExpressionWithTag "- condition:" (lexInfo expL)

            printString "- while:"
            raiseTabs >> do
                printStatements aftBlock
            lowerTabs
        lowerTabs

    StFor idnL expL block -> do
        printString $ "FOR " ++ show posn ++ ":"
        raiseTabs >> do
            printString $ "- variable: " ++ lexInfo idnL

            printExpressionWithTag "- range:" (lexInfo expL)

            printString "- body"
            raiseTabs >> do
                printStatements block
            lowerTabs
        lowerTabs


    StBreak    -> printString $ "BREAK"    ++ show posn
    StContinue -> printString $ "CONTINUE" ++ show posn

    _ -> error $ "Printer.printStatement: statement '" ++ show st ++ "' should not shown in the AST"

printExpression :: Expression -> Printer ()
printExpression exp = case exp of

    LitInt    i -> printString $ "Int literal: "   ++ show (lexInfo i)
    LitChar   c -> printString $ "Char literal: " ++ show (lexInfo c)
    LitBool   b -> printString $ "Bool literal: "   ++ map toLower (show (lexInfo b))
    LitFloat  f -> printString $ "Float literal: "     ++ show (lexInfo f)
    LitString s -> printString $ "String literal: "    ++ show (lexInfo s)

    Variable accL -> printString $ "Variable: " ++ show (lexInfo accL)

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

-- --
-- --  Literal printing
-- --
-- showIndex :: Expression -> String
-- showIndex = runPrinter . printExpressionIndex

-- printExpressionIndex :: Expression -> Printer ()
-- printExpressionIndex e = case e of
--     Variable accL -> printNonTerminalIndex $ show (lexInfo accL)
--     FunctionCall iden args -> do
--         printNonTerminalIndex (lexInfo iden ++ "(")
--         unless (null $ toList args) $ do
--             printExpressionIndex . lexInfo . head $ toList args
--             mapM_ func . tail $ toList args
--         printNonTerminalIndex ")"
--         where
--             func argL = printNonTerminalIndex ", " >> printExpressionIndex (lexInfo argL)
--     LitInt    i   -> printNonTerminalIndex $ show (lexInfo i)
--     LitChar   c   -> printNonTerminalIndex $ show (lexInfo c)
--     LitBool   b   -> printNonTerminalIndex $ show (lexInfo b)
--     LitFloat  f   -> printNonTerminalIndex $ show (lexInfo f)
--     LitString s _ -> printNonTerminalIndex $ show (lexInfo s)
--     ExpBinary op l r -> do
--         printExpressionIndex (lexInfo l)
--         printNonTerminalIndex " "
--         printBinary (lexInfo op)
--         printNonTerminalIndex " "
--         printExpressionIndex (lexInfo r)
--     ExpUnary op expr -> do
--         printUnary (lexInfo op)
--         printExpressionIndex (lexInfo expr)

-- printBinary :: Binary -> Printer ()
-- printBinary op = printNonTerminalIndex $ case op of
--     OpPlus    -> "+"
--     OpMinus   -> "-"
--     OpTimes   -> "-"
--     OpDivide  -> "/"
--     OpModulo  -> "%"
--     OpPower   -> "^"
--     OpFromTo  -> ".."
--     OpOr      -> "or"
--     OpAnd     -> "and"
--     OpEqual   -> "=="
--     OpUnequal -> "/="
--     OpLess    -> "<"
--     OpLessEq  -> "<="
--     OpGreat   -> ">"
--     OpGreatEq -> ">="
--     OpBelongs -> "@"

-- printUnary :: Unary -> Printer ()
-- printUnary op = printNonTerminalIndex $ case op of
--     OpNegate -> "-"
--     OpNot    -> "not"

-- printNonTerminalIndex :: String -> Printer ()
-- printNonTerminalIndex = tell . DS.singleton

printExpressionWithTag :: String -> Expression -> Printer ()
printExpressionWithTag tag exp = do
    printString tag
    raiseTabs
    printExpression exp
    lowerTabs

-- printStatements :: String -> StBlock -> Printer ()
-- printStatements tag is = do
--     printNonTerminal tag
--     raiseTabs >> mapM_ (printStatement . lexInfo) is >> lowerTabs

-- printPosn :: Position -> Printer ()
-- printPosn posn = do
--     t <- gets tabs
--     tell $ DS.singleton $ replicate t '\t' ++ showPosn posn ++ ":"

-- showDataType :: DataType -> String
-- showDataType dt = case dt of
--     Array aDtL indexL _ -> showDataType (lexInfo aDtL) ++ "[" ++ showIndex (lexInfo indexL) ++ "]"
--     _                   -> show dt
