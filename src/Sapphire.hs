module Main where

import           Checker            (CheckState (..), Checker, checkProgram,
                                     getErrors, runProgramChecker)
import           Parser

import           Control.Monad       (guard, void)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.Foldable       (mapM_)
import           Data.Sequence       as DS (null)
import           Prelude             as P hiding (mapM_)
import           System.Environment  (getArgs)

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
    let (lexE,parseE,staticE,afterW) = getErrors writer
    mapM_ print lexE
    mapM_ print parseE
    mapM_ print staticE
    mapM_ print afterW
    putStrLn "#################################################################\n"
    -- /TEMPORAL
    if DS.null writer
        then print stAst
        else do
            let (lexErrors,parseErrors,staticErrors,warnings) = getErrors writer
            -- Only print errors if there are no errors of more basic type
            void $ runMaybeT $ do
                liftIO $ mapM_ print lexErrors

                guard $ DS.null lexErrors
                liftIO $ mapM_ print parseErrors

                guard $ DS.null parseErrors
                liftIO $ mapM_ print staticErrors

                guard $ DS.null staticErrors
                liftIO $ mapM_ print warnings
