module Main where

import           Checker            (CheckState (..), Checker, checkProgram,
                                     getErrors, runProgramChecker)
import           Parser

import           Control.Monad      (when)
import           Data.Foldable      (mapM_)
import           Data.Sequence      as DS (null)
import           Prelude            as P hiding (mapM_)
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    input <- if P.null args
        then getContents
        else readFile $ head args
    printProgram $ uncurry checkProgram $ parseProgram input

printProgram :: Checker () -> IO ()
printProgram chk = do
    let (state,writer) = runProgramChecker chk
        CheckState stTable _ _ stAst _ = state
    -- TEMPORAL
    print state
    mapM_ print writer
    putStrLn "#################################################################\n"
    -- \TEMPORAL
    if DS.null writer
        then print stAst
        else do
            let (lexErrors,parseErrors,staticErrors,warnings) = getErrors writer
            mapM_ print lexErrors
            mapM_ print parseErrors
            mapM_ print staticErrors
            -- Only print warnings if there are no errors
            when (all DS.null [lexErrors,parseErrors, staticErrors]) $
                mapM_ print warnings

            -- Only print errors if there are no errors of more basic type
            --mapM_ print lexErrors
            --when (DS.null lexErrors) $ do
            --    mapM_ print parseErrors
            --    when (DS.null parseErrors) $ do
            --        mapM_ print staticErrors
            --        when (DS.null staticErrors) $
            --            mapM_ print warnings
