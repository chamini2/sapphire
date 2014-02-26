module Main where

import System.Environment (getArgs)

import Parser

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

--printer :: (a, [String]) -> IO ()
printer (st, ls) = if null ls then print st else mapM_ putStrLn ls

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

