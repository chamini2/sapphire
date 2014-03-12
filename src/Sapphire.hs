module Main where

import           Checker            (CheckState (..), checkProgram, getErrors,
                                     runProgramChecker)
import           Language           (Program (..))
import           Parser

import           Prelude
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    input <- if null args
        then getContents
        else readFile $ head args
    case parseProgram input of
        Right program -> printProgram program
        Left errors   -> mapM_ print $ reverse errors

printProgram :: Program -> IO ()
printProgram pr = do
    let (state,writer) = runProgramChecker $ checkProgram pr
        (Program prog) = ast state
    -- TEMPORAL
    print state
    mapM_ print writer
    mapM_ print prog
    putStrLn "--------------------------------------------------------------------------------"
    -- /TEMPORAL

    if null writer
        then mapM_ print prog
        else do
            let (lexErrors,parseErrors,staticErrors) = getErrors writer
            mapM_ print lexErrors
            mapM_ print parseErrors
            mapM_ print staticErrors
