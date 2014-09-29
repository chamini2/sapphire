module Main where

import           Definition
import           Parser
import           SappMonad
import           SizeOffset
import           TypeChecker

import           Control.Monad         (when, unless)
import           Data.Foldable         (mapM_)
import           Data.List             (nub)
import           Data.Sequence         (null)
import           Prelude               hiding (mapM_, null)
import qualified Prelude               as P (null)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (..),
                                        OptDescr (..), getOpt, usageInfo)
import           System.Environment    (getArgs)

main :: IO ()
main = do
    (flgs, args) <- arguments
    mapM_ print flgs
    when (Help    `elem` flgs) $ putStrLn help
    when (Version `elem` flgs) $ putStrLn version
    -- Only continue if there were no 'help' or 'version' flags
    unless ((Help `elem` flgs) || (Version `elem` flgs)) $ do
        input <- if P.null args
            then getContents
            else readFile $ head args
        let (readW, prog) = parseProgram input
        mapM_ print readW
        -- When there are no Lexing/Parsing errors
        when (null readW) $ do
            let (defS, defW) = processDefinition readW prog
            mapM_ print defW
            -- When there are no definition errors
            when (null defW) $ do
                let (typS, typW) = processTypeChecker defW (getTable defS) (getAst defS)
                mapM_ print typW
                -- When there are no type checking errors
                when (null typW) $ do
                    let (sizS, sizW) = processSizeOffset typW (getTable typS) (getAst typS)
                    print sizS
                    mapM_ print sizW
    putStrLn "done."

--------------------------------------------------------------------------------
-- Flags handling

options :: [OptDescr Flag]
options =
    [ Option "h" ["help"]    (NoArg  Help)              "shows this help message"
    , Option "v" ["version"] (NoArg  Version)           "shows version number"
    , Option "W" ["Wall"]    (NoArg  AllWarnings)       "show all warnings"
    , Option "w" ["Wnone"]   (NoArg  SuppressWarnings)  "suppress all warnings"
    , Option "o" ["output"]  (ReqArg OutputFile "FILE") "specify a FILE for output of the program"
    ]

help :: String
help = usageInfo "Usage: sapphire [OPTION]... [FILE]" options

version :: String
version = "sapphire 0.9"

arguments :: IO ([Flag], [String])
arguments = do
    args <- getArgs
    case getOpt Permute options args of
         (flgs,rest ,[]  ) -> return (nub $ coherent flgs, rest)
         (_   ,_    ,errs) -> ioError (userError (concat errs ++ help))
    where
        coherent = foldr func []
        func flg flgs = case flg of
            Help             -> flg : flgs
            Version          -> flg : flgs
            AllWarnings      -> if SuppressWarnings `elem` flgs then flgs else flg : flgs
            SuppressWarnings -> if AllWarnings      `elem` flgs then flgs else flg : flgs
            OutputFile _     -> flg : flgs
