module Main where

import Control.Monad

import Lib

main :: IO ()
main = do
    program <- getContents
    let tokens = scanTokens program

    putStrLn "TOKENS:"
    forM_ tokens $ \tk -> do
        putStr $ prettyShow (posn tk)
        putStr ": "
        putStrLn $ prettyShow tk

    putStrLn "\nAST:"
    print $ parseProgram tokens
