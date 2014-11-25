{-# LANGUAGE TupleSections #-}
module Main where

import           Language.Sapphire.Definition
-- import           Language.Sapphire.MIPSGenerator
import           Language.Sapphire.Parser
import           Language.Sapphire.SappMonad
import           Language.Sapphire.SizeOffset
import           Language.Sapphire.TACGenerator
import           Language.Sapphire.TypeChecker

import           Control.Monad                   (guard, void, when)
import           Control.Monad.Trans             (liftIO)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import           Data.Foldable                   (mapM_, toList)
import           Data.List                       (nub)
import           Data.Sequence                   (null)
import           Prelude                         hiding (mapM_, null)
import qualified Prelude                         as P (null)
import           System.Console.GetOpt           (ArgDescr (..), ArgOrder (..),
                                                  OptDescr (..), getOpt,
                                                  usageInfo)
import           System.Directory                (removeFile)
import           System.Environment              (getArgs)
import           System.FilePath                 (replaceExtension,
                                                  takeFileName)
import           System.Process                  (rawSystem)

main :: IO ()
main = void $ runMaybeT $ do
    (flgs, args) <- liftIO arguments

    when (Version `elem` flgs) . liftIO $ putStrLn version
    when (Help    `elem` flgs) . liftIO $ putStr help

    -- Only continue if there were no 'help' or 'version' flags
    guard . not $ (Help `elem` flgs) || (Version `elem` flgs)

    let fromStdin = P.null args
    (input, filepath) <- if fromStdin
        then liftIO getContents            >>= return . (, "<stdin>")
        else liftIO (readFile $ head args) >>= return . (, head args)

    let reader = initialReader { file = filepath, flags = flgs }

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

    let (tacS, blocks) = processTACGenerator (getTable sizS) prog

    -- let mipsc = processMIPSGenerator reader (getTable tacS) blocks
    let mipsc = [1,2,3]
        mipsf = flip replaceExtension "s" $ takeFileName filepath

    when (ShowSymbolTable `elem` flgs) . liftIO $ print (getTable sizS)
    when (ShowAST         `elem` flgs) . liftIO $ print prog
    when (ShowTAC         `elem` flgs) . liftIO $ mapM_ (mapM_ print) blocks
    when (ShowMIPS        `elem` flgs) . liftIO $ mapM_ print mipsc

    liftIO . writeFile mipsf . unlines . map show $ toList mipsc

    -- Run the MIPS file
    when (Execute `elem` flgs || fromStdin) . void . liftIO $ rawSystem "spim" ["-f", mipsf]

    when fromStdin . liftIO $ removeFile mipsf

--------------------------------------------------------------------------------
-- Flags handling

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]         (NoArg Help)              "shows this help message"
    , Option ['v'] ["version"]      (NoArg Version)           "shows version number"
    , Option ['W'] ["all-warnings"] (NoArg AllWarnings)       "show all warnings"
    , Option ['w'] ["no-warnings"]  (NoArg SuppressWarnings)  "suppress all warnings"
    , Option ['s'] ["symbol-table"] (NoArg ShowSymbolTable)   "shows the symbol table"
    , Option ['a'] ["ast"]          (NoArg ShowAST)           "shows the AST"
    , Option ['t'] ["tac"]          (NoArg ShowTAC)           "shows the three-address code generated"
    , Option ['m'] ["mips"]         (NoArg ShowMIPS)          "shows the MIPS code generated"
    , Option ['e'] ["execute"]      (NoArg Execute)           "execute the MIPS code directly with the 'spim' command"
    ]

help :: String
help = usageInfo message options
    where
        message = "usage: sapphire [OPTION]... [FILE]\n" ++
                  "\twhen running sapphire without arguments, the compiler " ++
                  "consumes data it\n\treceives from the standard input until " ++
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
            AllWarnings      ->
                if SuppressWarnings `elem` flgs
                    then flgs
                    else flg : flgs
            SuppressWarnings ->
                if AllWarnings `elem` flgs
                    then flgs
                    else flg : flgs
            _ -> flg : flgs
