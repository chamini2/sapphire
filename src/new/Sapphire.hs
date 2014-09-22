module Main where

--import           Checker             (CheckState (..), Checker, checkProgram, getErrors, runProgramChecker)
import           Definition
import           TypeChecker
import           Parser

import           Control.Monad       (when)
--import           Control.Monad.Trans (lift)
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
    let (defS, defW) = uncurry processDefinition $ parseProgram input
    print defS
    mapM_ print defW
    when (DS.null defW) $ do
        let (typS, typW) = processTypeChecker defW (table defS) (ast defS)
        print typS
        mapM_ print typW
    putStrLn "done."
