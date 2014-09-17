{-# LANGUAGE FlexibleContexts #-}
module SappMonad where

import           Error
import           Program
import           SymbolTable

import           Control.Monad        (MonadPlus, when, liftM, unless, guard)
import           Control.Monad.State  (MonadState, gets, modify)
import           Control.Monad.Writer (MonadWriter, tell)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import qualified Data.Map             as DM (Map, fromList)
import           Data.Maybe           (isJust)
import           Data.Sequence        as DS (Seq, empty, singleton)

--------------------------------------------------------------------------------
-- Monadic functions

unlessGuard :: MonadPlus m => Bool -> m () -> m ()
unlessGuard cond actn = unless cond actn >> guard cond

--------------------------------------------------------------------------------
-- Reader

data SappReader = SappReader
    { flags :: Seq Flag
    , arch  :: Architecture
    }

data Flag = OutputFile FilePath | SupressWarnings | AllWarnings
    deriving (Eq)

data Architecture = Arch
    { archName :: String
    , types    :: DM.Map DataType Bytes
    } deriving (Show)

----------------------------------------
-- Instances

instance Eq Architecture where
    (==) = (==) `on` archName

----------------------------------------
-- Initial

initialReader :: SappReader
initialReader = SappReader
    { flags  = empty
    , arch   = defaultArchitecture
    }

defaultArchitecture :: Architecture
defaultArchitecture = Arch
    { archName = "mips"
    , types = DM.fromList
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

--------------------------------------------------------------------------------
-- State

class SappState s where
    getTable   :: s -> SymbolTable
    getStack   :: s -> Stack Scope
    getScopeId :: s -> ScopeNum
    getAst     :: s -> Program
    putTable   :: SymbolTable -> s -> s
    putStack   :: Stack Scope -> s -> s
    putScopeId :: ScopeNum    -> s -> s
    putAst     :: Program     -> s -> s


--------------------------------------------------------------------------------
-- Error reporting

tellLError :: MonadWriter (Seq Error) m => Position -> LexerError -> m ()
tellLError posn err = tell (singleton $ LError posn err)

tellPError :: MonadWriter (Seq Error) m => Position -> ParseError -> m ()
tellPError posn err = tell (singleton $ PError posn err)

tellSError :: MonadWriter (Seq Error) m => Position -> StaticError -> m ()
tellSError posn err = tell (singleton $ SError posn err)

tellWarn :: MonadWriter (Seq Error) m => Position -> Warning -> m ()
tellWarn posn err = tell (singleton $ Warn posn err)

--------------------------------------------------------------------------------
-- Scope Handling

enterScope :: (SappState s, MonadState s m) => m ()
enterScope = do
    currentId <- gets getScopeId
    let scope = Scope { serial = currentId + 1 }
    modify $ \s -> putStack (push scope (getStack s)) $ putScopeId (currentId + 1) s

exitScope :: (SappState s, MonadState s m) => m ()
exitScope = modify $ \s -> putStack (pop $ getStack s) s

currentScope :: (SappState s, MonadState s m) => m ScopeNum
currentScope = gets (serial . top . getStack)

--------------------------------------------------------------------------------
-- SymbolTable

----------------------------------------
-- Symbol

addSymbol :: (SappState s, MonadState s m)
          => Identifier -> Symbol -> m ()
addSymbol idn sym = do
    tab <- gets getTable
    modify $ \s -> putTable (insert idn sym tab) s

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
getsSymbolWithStack idn stk f = do
    tab <- gets getTable
    return $ f <$> lookupWithScope idn stk tab -- f <$> == maybe Nothing (Just . f)

modifySymbolWithScopeNStack :: (SappState s, MonadState s m)
                            => Identifier -> ScopeNum -> Stack Scope -> (Symbol -> Symbol) -> m ()
modifySymbolWithScopeNStack idn scope stk f = do
    tab <- gets getTable
    exists <- liftM isJust $ getsSymbolWithStack idn stk f
    when exists $ modify $ \s -> putTable (updateWithScope idn scope f tab) s

modifySymbolWithScope :: (SappState s, MonadState s m)
                      => Identifier -> ScopeNum -> (Symbol -> Symbol) -> m ()
modifySymbolWithScope idn scope f = do
    stk <- gets getStack
    modifySymbolWithScopeNStack idn scope stk f

modifySymbol :: (SappState s, MonadState s m)
             => Identifier -> (Symbol -> Symbol) -> m ()
modifySymbol idn f = do
    mayScopeN <- getsSymbol idn scopeNum
    case mayScopeN of
        Nothing -> return ()
        Just symScopeN -> modifySymbolWithScope idn symScopeN f

----------------------------------------
-- Used

markUsed :: (SappState s, MonadState s m)
         => Identifier -> m ()
markUsed idn = modifySymbol idn (\sym -> sym { used = True })
