{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
module Checker where

--import           Lexer
import           Language

--import           Control.Monad.Error    (Error(..))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS
import           Data.Typeable          (Typeable (..))

type Program  = Checker [Statement]

--type Checker a = ErrorT LexerError (RWST CheckReader CheckWriter CheckState Identity) a
type Checker a = RWST CheckReader CheckWriter CheckState Identity a

data Flag = OutputFile String | SupressWarnings
type CheckReader = [Flag]

----------------------------------------

data CheckError where
    LError :: LexerError  {- -> AlexPosn -} -> CheckError
    PError :: ParseError  {- -> AlexPosn -} -> CheckError
    SError :: StaticError {- -> AlexPosn -} -> CheckError
    deriving (Show, Typeable)

data LexerError where
    UnexpectedChar :: Char   -> LexerError
    StringError    :: String -> LexerError
    LexerError     :: String -> LexerError

instance Show LexerError where
    show (UnexpectedChar c) = "Unexpected character: '" ++ [c] ++ "'"
    show (StringError str)  = "Missing matching '\"' for string '" ++ str ++ "'"
    show (LexerError  msg)  = "Lexing error: " ++ msg

data ParseError where
    UnexpectedToken :: {-Token-} String  -> ParseError
    ParseError      :: String -> ParseError

instance Show ParseError where
    show (UnexpectedToken tok) = "Parsing error on Token: " ++ show tok
    show (ParseError msg)      = "Parsing error: " ++ msg

data StaticError where
    BinaryTypes :: Binary -> [DataType] -> StaticError
    UnaryTypes  :: Unary  -> DataType   -> StaticError
    StaticError :: String -> StaticError

instance Show StaticError where
    show (UnaryTypes op dt)   = "Static error: operator '" ++ show op ++ "' doesn't work with arguments '" ++ show dt ++ "'"
    show (BinaryTypes op dts) = "Static error: operator '" ++ show op ++ "' doesn't work with arguments " ++  take 2 (concatMap (\dt -> ", '" ++ show dt) dts) ++ "'"
    show (StaticError msg)    = "Static error: " ++ msg

----------------------------------------

type CheckWriter = [CheckError]

data CheckState = CheckState
    { symtable :: ()-- SymTable
    , ast      :: ()-- Program
    }
    --deriving (Show)

--instance Error LexerError where
--    noMsg  = UnexpectedChar "Lexical error"
--    strMsg = UnexpectedChar

flags :: CheckReader
flags = []

initialState :: CheckState
initialState = CheckState () ()

----------------------------------------


--checkBinary :: Expression -> Expression -> Checker Program

--runChecker :: Checker a -> (Either LexerError a, CheckState, CheckWriter)
--runChecker = runIdentity . flip (`runRWST` flags) initialState . runErrorT
runChecker :: Checker a -> (a, CheckState, CheckWriter)
runChecker = runIdentity . flip (`runRWST` flags) initialState

getWriter :: Checker a -> CheckWriter
getWriter = (\(_,_,w) -> w) . runChecker

--getCheck :: Checker a -> Either LexerError a
--getCheck = (\(c,_,_) -> c) . runChecker
getCheck :: Checker a -> a
getCheck = (\(c,_,_) -> c) . runChecker

getState :: Checker a -> CheckState
getState = (\(_,s,_) -> s) . runChecker

getErrors :: CheckWriter -> ([LexerError], [ParseError], [StaticError])
getErrors errors = (lexErrors, parseErrors, staticErrors)
    where
        lexErrors    = map (\(LError e) -> e) $ filter ((=="LError") . show . typeOf) errors
        parseErrors  = map (\(PError e) -> e) $ filter ((=="PError") . show . typeOf) errors
        staticErrors = map (\(SError e) -> e) $ filter ((=="SError") . show . typeOf) errors
