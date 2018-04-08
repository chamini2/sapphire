module Main where

import Lib

main :: IO ()
main = do
    program <- getContents
    let tokens = scanTokens program
    print $ tokens
    print $ parseProgram tokens
