{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
module Checker where

--import           Lexer
import           Language
import           SymbolTable

--import           Control.Monad.Error    (Error(..))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS
import           Data.Typeable          (Typeable (..))
import           Prelude hiding (lookup)

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
    { table :: SymTable
    , stack :: Stack Scope
    {-, ast   :: ()-- Program-}
    } deriving (Show)

--instance Error LexerError where
--    noMsg  = UnexpectedChar "Lexical error"
--    strMsg = UnexpectedChar

flags :: CheckReader
flags = []

initialState :: CheckState
initialState = CheckState { table = emptyTable , stack = emptyStack }

{-|
    Entrando a un nuevo scope
-}
enterScope :: Checker ()
enterScope = do
    cs <- getCurrentScope 
    ss <- gets stack
    let sc = Scope { serial = cs + 1 , closed = False }
    modify (\s -> s { stack = push sc ss })

{-|
    Saliendo de un scope ya recorrido completamente
-}
exitScope :: Checker ()
exitScope = do
    ss <- gets stack
    {-let (top, rest) = pop ss-}
    modify (\s -> s { stack = snd $ pop ss } )

{-|
    Consigue el serial del scope actual
-}
getCurrentScope :: Checker ScopeNum
getCurrentScope = liftM (serial . fst . pop) (gets stack) 

{-|
    Adds a symbol to the Checker's symbol table
-}
addSymbol :: Identifier -> SymInfo -> Checker ()
addSymbol id info = modify (\s -> s { table = insert id info (table s)})

{-|
    Gets a symbol's value if it has one or Nothing otherwise
-}
getSymbolValue :: Identifier -> (SymInfo -> a) -> Checker a
getSymbolValue id f = do
    table <- gets table
    maybe fail success $ lookup id table 
    where success = return . f 
          fail    = undefined

{-|
    Cambia el valor de la variable id por el valor vn en
    la tabla de simbolos
-}
changeValue :: Identifier -> Value -> Checker ()
changeValue id vn = undefined

{-|
    Marcamos a la variable id como bloqueada en la tabla de simbolos
-}
{-marcarVariableOcupada :: Identifier -> Checker ()-}
{-marcarVariableOcupada id = do-}
    {-tabla <- gets tabla-}
    {-case buscarInfoSim id tabla of-}
        {-Just info -> do-}
            {-eliminarDeclaracion $ Decl id (tipo info) -}
            {-agregarSimbolo id $ info { ocupado = True } -}
        {-otherwise -> continue-}

{-|
    Agregamos a la tabla de simbolos las variables definidas en ds
    y aumentamos el scope actual
-}
processDeclarationList :: [Declaration] -> Checker () 
processDeclarationList ds = enterScope >> mapM_ processDeclaration ds >> exitScope

{-|
    Agregamos la variable v de tipo tn a la tabla de simbolos 
-}
processDeclaration :: Declaration -> Checker ()
processDeclaration (Declaration id t) = do
    table <- gets table
    cs    <- getCurrentScope
    let info = SymInfo {
                 dataType = t,
                 value    = Nothing,
                 scopeNum = cs,
                 line     = 0,
                 column   = 0
               }
    case lookup id table of 
        Nothing                              -> addSymbol id info
        Just (SymInfo _ _ l _ _) | l == cs   -> return () --throwError $ MultipleDeclarations id 
                                 | otherwise -> addSymbol id info

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
