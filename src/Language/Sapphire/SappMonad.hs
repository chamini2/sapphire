{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Language.Sapphire.SappMonad where

import           Language.Sapphire.Error
import           Language.Sapphire.Printer ()   -- For the Show instance of Program
import           Language.Sapphire.Program
import           Language.Sapphire.SymbolTable

import           Control.Monad        (MonadPlus, when, liftM, unless, guard)
import           Control.Monad.State  (MonadState, gets, modify)
import           Control.Monad.Writer (MonadWriter, tell)
import           Data.Function        (on)
import qualified Data.Map.Strict      as Map (Map, fromList)
import           Data.Maybe           (isJust)
import           Data.Sequence        (Seq, singleton, empty)

--------------------------------------------------------------------------------
-- Monadic functions

unlessGuard :: MonadPlus m => Bool -> m () -> m ()
unlessGuard cond actn = unless cond actn >> guard cond

--------------------------------------------------------------------------------
-- Reader

data SappReader = SappReader
    { flags :: [Flag]
    , arch  :: Architecture
    }

data Flag = Help                    -- -h      | --help
          | Version                 -- -v      | --version
          | AllWarnings             -- -W      | --all-warnings
          | SuppressWarnings        -- -w      | --no-warnings
          | OutputFile FilePath     -- -o FILE | --output FILE
          -- For compiler use
          | ShowSymbolTable         -- -st     | --symbol-table
          | ShowAST                 -- -a      | --ast
    deriving (Show)

data Architecture = Arch
    { archName :: String
    , types    :: Map.Map DataType Width
    } deriving (Show)

----------------------------------------
-- Instances

instance Eq Flag where
    a == b = case (a, b) of
        (Help            , Help            ) -> True
        (Version         , Version         ) -> True
        (AllWarnings     , AllWarnings     ) -> True
        (SuppressWarnings, SuppressWarnings) -> True
        (OutputFile _    , OutputFile _    ) -> True
        (ShowSymbolTable , ShowSymbolTable ) -> True
        (ShowAST         , ShowAST         ) -> True
        (_               , _               ) -> False

instance Eq Architecture where
    (==) = (==) `on` archName

----------------------------------------
-- Initial

initialReader :: SappReader
initialReader = SappReader
    { flags  = []
    , arch   = defaultArchitecture
    }

defaultArchitecture :: Architecture
defaultArchitecture = Arch
    { archName = "mips"
    , types = Map.fromList
        [ (Int     , 4)
        , (Float   , 4)
        , (Char    , 1)
        , (Bool    , 1)
        --, (Pointer , 4)
        ]
    }

--------------------------------------------------------------------------------
-- Writer

type SappWriter = Seq Error

----------------------------------------
-- Initial

initialWriter :: SappWriter
initialWriter = empty

--------------------------------------------------------------------------------
-- State

class Show s => SappState s where
    getTable   :: s -> SymbolTable
    getStack   :: s -> Stack Scope
    getScopeId :: s -> Scope
    getAst     :: s -> Program
    putTable   :: SymbolTable -> s -> s
    putStack   :: Stack Scope -> s -> s
    putScopeId :: Scope    -> s -> s
    putAst     :: Program     -> s -> s

----------------------------------------
-- Instances

showSappState :: SappState s => s -> String
showSappState st = showT ++ showS ++ showA
    where
        showT = show (getTable st) ++ "\n"
        showS = "Scope Stack:\n"  ++ show (getStack st) ++ "\n"
        showA = show (getAst st) ++ "\n"

--------------------------------------------------------------------------------
-- Error reporting

tellLError :: MonadWriter SappWriter m => Position -> LexerError -> m ()
tellLError posn = tell . singleton . LError posn

tellPError :: MonadWriter SappWriter m => Position -> ParseError -> m ()
tellPError posn = tell . singleton . PError posn

tellSError :: MonadWriter SappWriter m => Position -> StaticError -> m ()
tellSError posn = tell . singleton . SError posn

tellWarn :: MonadWriter SappWriter m => Position -> Warning -> m ()
tellWarn posn = tell . singleton . Warn posn

--------------------------------------------------------------------------------
-- Scope Handling

enterScope :: (SappState s, MonadState s m) => m ()
enterScope = do
    scp <- liftM succ $ gets getScopeId
    modify $ \s -> putStack (push scp (getStack s)) $ putScopeId scp s

exitScope :: (SappState s, MonadState s m) => m ()
exitScope = modify $ \s -> putStack (pop $ getStack s) s

currentScope :: (SappState s, MonadState s m) => m Scope
currentScope = gets (top . getStack)

--------------------------------------------------------------------------------
-- SymbolTable

addSymbol :: (SappState s, MonadState s m)
          => Identifier -> Symbol -> m ()
addSymbol idn sym = do
    tab <- gets getTable
    modify $ \s -> putTable (insert idn sym tab) s

----------------------------------------

getSymbol :: (SappState s, MonadState s m)
          => Identifier -> m (Maybe Symbol)
getSymbol = flip getsSymbol id

getsSymbol :: (SappState s, MonadState s m)
           => Identifier -> (Symbol -> a) -> m (Maybe a)
getsSymbol idn f = do
    stk <- gets getStack
    getsSymbolWithStack idn stk f

getsSymbolWithStack :: (SappState s, MonadState s m)
                    => Identifier -> Stack Scope -> (Symbol -> a) -> m (Maybe a)
getsSymbolWithStack idn stk f = gets getTable >>= return . fmap f . lookupWithScope idn stk

----------------------------------------

modifySymbolWithScope :: (SappState s, MonadState s m)
                      => Identifier -> Stack Scope -> (Symbol -> Symbol) -> m ()
modifySymbolWithScope idn stk f = do
    tab <- gets getTable
    exists <- liftM isJust $ getsSymbolWithStack idn stk f
    when exists $ modify $ \s -> putTable (updateWithScope idn stk f tab) s

modifySymbol :: (SappState s, MonadState s m)
             => Identifier -> (Symbol -> Symbol) -> m ()
modifySymbol idn f = getsSymbol idn scopeStack >>= \case
    Nothing  -> return ()
    Just stk -> modifySymbolWithScope idn stk f

----------------------------------------
-- Used

markUsed :: (SappState s, MonadState s m)
         => Identifier -> m ()
markUsed idn = modifySymbol idn $ \sym -> sym { used = True }
