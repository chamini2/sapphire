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
        (Right statements) -> mapM_ print statements
        (Left error) -> print error
