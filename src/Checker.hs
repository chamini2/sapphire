{-# LANGUAGE GADTs #-}
module Checker where

--import           Lexer
import           Language
import           SymbolTable

--import           Control.Monad.Error    (Error(..))
import           Control.Arrow          ((&&&))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS
import           Data.List              (sortBy)
import           Prelude                hiding (lookup)

type Program  = Checker [Statement]

--type Checker a = ErrorT LexerError (RWST CheckReader CheckWriter CheckState Identity) a
type Checker a = RWST CheckReader CheckWriter CheckState Identity a

data Flag = OutputFile String | SupressWarnings
type CheckReader = [Flag]

----------------------------------------

data CheckError where
    LError :: Position -> LexerError  -> CheckError
    PError :: Position -> ParseError  -> CheckError
    SError :: Position -> StaticError -> CheckError
    deriving (Show)

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
    -- Variables
    VariableNotDeclared    :: Identifier -> StaticError
    VariableNotInitialized :: Identifier -> StaticError
    InvalidAssignType      :: Identifier -> DataType -> DataType -> StaticError
    -- Operators
    BinaryTypes :: Binary -> [DataType] -> StaticError
    UnaryTypes  :: Unary  -> DataType   -> StaticError
    -- General
    StaticError :: String -> StaticError

instance Show StaticError where
    show (VariableNotDeclared id)     = "Static error: variable '" ++ id ++ "' has not been declared"
    show (VariableNotInitialized id)  = "Static error: variable '" ++ id ++ "' has not been initialized"
    show (InvalidAssignType id vt et) = "Static error: cant assign expression of type '" ++ show et ++ "' to variable '" ++ id ++ "' of type '" ++ show vt ++ "'"
    show (UnaryTypes op dt)   = "Static error: operator '" ++ show op ++ "' doesn't work with arguments '" ++ show dt ++ "'"
    show (BinaryTypes op dts) = "Static error: operator '" ++ show op ++ "' doesn't work with arguments " ++ take 2 (concatMap (\dt -> ", '" ++ show dt) dts) ++ "'"
    show (StaticError msg)    = "Static error: " ++ msg

----------------------------------------

type CheckWriter = [CheckError]

data CheckState = CheckState
    { table    :: SymTable
    , stack    :: Stack Scope
    , currtSc  :: ScopeNum
    {-, ast   :: ()-- Program-}
    , currPosn :: Position
    } deriving (Show)

--instance Error LexerError where
--    noMsg  = UnexpectedChar "Lexical error"
--    strMsg = UnexpectedChar

flags :: CheckReader
flags = []

initialState :: CheckState
initialState = CheckState
    { table    = emptyTable
    , stack    = emptyStack
    , currtSc  = 0
    , currPosn = (1,1)
    }

{-|
    Entrando a un nuevo scope
-}
enterScope :: Checker ()
enterScope = do
    cs <- gets currtSc
    let sc    = Scope { serial = cs + 1 }
        newCs = cs + 1
    modify (\s -> s { stack = push sc (stack s), currtSc = cs + 1 })

{-|
    Saliendo de un scope ya recorrido completamente
-}
exitScope :: Checker ()
exitScope = modify (\s -> s { stack = snd . pop $ stack s })

{-|
    Adds a symbol to the Checker's symbol table
-}
addSymbol :: Identifier -> SymInfo -> Checker ()
addSymbol id info = modify (\s -> s { table = insert id info (table s)})

{-|
    Gets a symbol's value if it has one or Nothing otherwise
-}
getSymInfoArg :: Identifier -> (SymInfo -> a) -> Checker a
getSymInfoArg id f = do
    tab <- gets table
    maybe fail success $ lookup id tab
    where
        success = return . f
        fail    = do
            posn <- gets currPosn
            tell [SError posn $ VariableNotDeclared id]
            return $ f emptySymInfo

{-|
    Cambia el valor de la variable id por el valor vn en
    la tabla de simbolos
-}
modifyValue :: Identifier -> Value -> Checker ()
modifyValue id v = do
    tab <- gets table
    let mSym = lookup id tab
    case mSym of
        Just _  -> modify $ func tab
        Nothing -> do
            posn <- gets currPosn
            tell [SError posn $ VariableNotDeclared id]
    where
        func tab s = s { table = update id modValue tab }
        modValue sy = sy { value = Just v }
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
    Adds the declaration of id to the symbol table
-}
processDeclaration :: Declaration -> Checker ()
processDeclaration (Declaration id t c) = do
    posn <- gets currPosn
    tab  <- gets table
    cs   <- gets currtSc
    let info = SymInfo {
                 dataType = t,
                 category = c,
                 value    = Nothing,
                 scopeNum = cs,
                 declPosn = posn
               }
    case lookup id tab of
        Nothing                              -> addSymbol id info
        Just (SymInfo _ _ _ l _) | l == cs   -> return () --throwError $ MultipleDeclarations id
                                 | otherwise -> addSymbol id info

checkExpression :: Expression -> Checker DataType
checkExpression (Variable id)   = do
    (val, dt) <- getSymInfoArg id (value &&& dataType) -- (\si -> (value si, dataType si))
    case val of
        Just _  -> return dt
        Nothing -> do
            posn <- gets currPosn
            tell [SError posn $ VariableNotInitialized id]
            return dt
checkExpression (LitInt _)      = return Int
checkExpression (LitFloat _)    = return Float
checkExpression (LitBool _)     = return Bool
checkExpression (LitChar _)     = return Char
checkExpression (LitString _)   = return String
checkExpression (ExpBinary op l r) = do
    ldt  <- checkExpression l
    rdt  <- checkExpression r
    let dts = filter (((ldt,rdt)==) . fst) $ binaryOperation op
    if null dts
        then do
            posn <- gets currPosn
            tell [SError posn $ BinaryTypes op [ldt,rdt]]
            return . snd . head $ binaryOperation op
        else return . snd $ head dts
checkExpression (ExpUnary op e)    = do
    dt   <- checkExpression e
    let dts = filter ((dt==) . fst) $ unaryOperation op
    if null dts
        then do
            posn <- gets currPosn
            tell [SError posn $ UnaryTypes op dt]
            return . snd . head $ unaryOperation op
        else return . snd $ head dts


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

getErrors :: CheckWriter -> ([(LexerError, Position)], [(ParseError, Position)], [(StaticError, Position)])
getErrors errors = (lexErrors, parseErrors, staticErrors)
    where
        lexErrors    = sortBy posSort . map (\(LError p e) -> (e,p)) $ filter funcLError errors
        parseErrors  = sortBy posSort . map (\(PError p e) -> (e,p)) $ filter funcPError errors
        staticErrors = sortBy posSort . map (\(SError p e) -> (e,p)) $ filter funcSError errors
        posSort (_,l) (_,r) = l `compare` r
        funcLError e = case e of
            (LError p e) -> True
            _            -> False
        funcPError e = case e of
            (PError p e) -> True
            _            -> False
        funcSError e = case e of
            (SError p e) -> True
            _            -> False
