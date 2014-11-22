{-# LANGUAGE TupleSections #-}
module Main where

import           Language.Sapphire.Definition
import           Language.Sapphire.MIPSGenerator
import           Language.Sapphire.Parser
import           Language.Sapphire.SappMonad
import           Language.Sapphire.SizeOffset
import           Language.Sapphire.TACGenerator
import           Language.Sapphire.TypeChecker

import           Control.Monad                 (guard, void, when)
import           Control.Monad.Trans           (liftIO)
import           Control.Monad.Trans.Maybe     (runMaybeT)
import           Data.Foldable                 (mapM_)
import           Data.List                     (nub)
import           Data.Sequence                 (null)
import           Prelude                       hiding (mapM_, null)
import qualified Prelude                       as P (null)
import           System.Console.GetOpt         (ArgDescr (..), ArgOrder (..),
                                                OptDescr (..), getOpt,
                                                usageInfo)
import           System.Environment            (getArgs)

main :: IO ()
main = void $ runMaybeT $ do
    (flgs, args) <- liftIO arguments

    when (Version `elem` flgs) . liftIO $ putStrLn version
    when (Help    `elem` flgs) . liftIO $ putStrLn help

    -- Only continue if there were no 'help' or 'version' flags
    guard . not $ (Help `elem` flgs) || (Version `elem` flgs)

    (input, file') <- if P.null args
        then liftIO getContents            >>= return . (, "<stdin>")
        else liftIO (readFile $ head args) >>= return . (, head args)

    let reader = initialReader { file = file', flags = flgs }

    let (prog, plErrs) = parseProgram input
    unlessGuard (null $ errors plErrs) $ mapM_ (liftIO . print) plErrs
    -- When there are no lexer or parser errors

    let (defS, dfErrs) = processDefinition reader plErrs prog
    unlessGuard (null $ errors dfErrs) $ mapM_ (liftIO . print) dfErrs
    -- When there are no definition errors

    let (typS, tpErrs) = processTypeChecker reader dfErrs (getTable defS) prog
    unlessGuard (null $ errors tpErrs) $ mapM_ (liftIO . print) tpErrs
    -- When there are no type checking errors

    let (sizS, szErrs) = processSizeOffset reader tpErrs (getTable typS) prog
    unlessGuard (null $ errors szErrs) $ mapM_ (liftIO . print) szErrs
    -- When there are no size or offset errors

    mapM_ (liftIO . print) $ warnings szErrs

    when (ShowSymbolTable `elem` flgs) . liftIO $ print (getTable sizS)
    when (ShowAST         `elem` flgs) . liftIO $ print prog

    let (tacS, tac) = processTACGenerator () (getTable sizS) prog

    when (ShowTAC `elem` flgs) . liftIO $ (mapM_ print tac)
    
    let mipsCode = processMIPSGenerator reader (getTable tacS) tac

    mapM_ (liftIO . print) mipsCode

    {-liftIO $ putStrLn "done."-}

--------------------------------------------------------------------------------
-- Flags handling

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]         (NoArg  Help)              "shows this help message"
    , Option ['v'] ["version"]      (NoArg  Version)           "shows version number"
    , Option ['W'] ["all-warnings"] (NoArg  AllWarnings)       "show all warnings"
    , Option ['w'] ["no-warnings"]  (NoArg  SuppressWarnings)  "suppress all warnings"
    , Option ['o'] ["output"]       (ReqArg OutputFile "FILE") "specify a FILE for output of the program"
    , Option ['s'] ["symbol-table"] (NoArg  ShowSymbolTable)   "shows the symbol table"
    , Option ['a'] ["ast"]          (NoArg  ShowAST)           "shows the AST"
    , Option ['t'] ["tac"]          (NoArg  ShowTAC)           "shows the three-address code generated"
    ]

help :: String
help = usageInfo message options
    where
        message = "usage: sapphire [OPTION]... [FILE]\n" ++
                  "\twhen running sapphire without arguments, the compiler " ++
                  "consumes data it receives from the standard input until " ++
                  "it receives an EOF ('^D') character"

version :: String
version = "sapphire 0.1.0.0"

arguments :: IO ([Flag], [String])
arguments = do
    args <- getArgs
    case getOpt Permute options args of
         (flgs,rest,[]  ) -> return (nub $ coherent flgs, rest)
         (_   ,_   ,errs) -> ioError (userError (concat errs ++ help))
    where
        coherent = foldr func []
        func flg flgs = case flg of
            Help             -> flg : flgs
            Version          -> flg : flgs
            AllWarnings      ->
                if SuppressWarnings `elem` flgs
                    then flgs
                    else flg : flgs
            SuppressWarnings ->
                if AllWarnings `elem` flgs
                    then flgs
                    else flg : flgs
            OutputFile _     -> flg : flgs
            ShowSymbolTable  -> flg : flgs
            ShowAST          -> flg : flgs
            ShowTAC          -> flg : flgs
