module Main where

import System.Environment (getArgs)
import Data.Either (lefts, rights)

import Parser
import Language

main :: IO ()
main = do
    args <- getArgs
    input <- if null args
        then getContents
        else readFile (head args)
    let result = parseProgram input
    case result of
        (Right program) -> mapM_ (printer . runChecker) program
        (Left errors)   -> print errors

--printer :: (Either LexError Statement, CheckState, CheckWriter) -> IO ()
printer (either, state, writer) =
    case either of
        Left lexError   -> print lexError
        Right statement -> if null writer
            then print statement
            else do
                let parseErrors  = lefts writer
                    staticErrors = rights writer
                mapM_ print parseErrors
                mapM_ print staticErrors

--main :: IO ()
--main = do
--    args <- getArgs
--    input <- if null args
--        then getContents
--        else readFile (head args)
--    let result = parseProgram input
--    case result of
--        (Right statements) -> do
--            putStr "Program\n"
--            mapM_ (putStr . treePrint 1) statements
--        (Left error) -> print error

