module Main where

import           Checker            (CheckState (..), Checker, checkProgram, getErrors,
                                     runProgramChecker)
import           Parser

import           Prelude
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    input <- if null args
        then getContents
        else readFile $ head args
    printProgram $ uncurry checkProgram $ parseProgram input

printProgram :: Checker () -> IO ()
printProgram chk = do
    let (state,writer) = runProgramChecker chk
        CheckState stTable _ _ stAst = state
    -- TEMPORAL
    print state
    mapM_ print writer
    putStrLn "#################################################################\n"
    -- \TEMPORAL

    if null writer
        then print stAst
        else do
            let (lexErrors,parseErrors,staticErrors) = getErrors writer
            mapM_ print lexErrors
            mapM_ print parseErrors
            mapM_ print staticErrors
