module Main where

import           Language
import           Parser

import           Prelude
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    input <- if null args
        then getContents
        else readFile (head args)
    case parseProgram input of
        Right program -> printProgram program
        _             -> putStrLn "IMPOSSIBLE ERROR!"
        --Left errors   -> print errors

printProgram :: Program -> IO ()
printProgram program = do
    let (statements,_,writer) = runChecker program
    if null writer
        then mapM_ print statements
        else mapM_ print writer
