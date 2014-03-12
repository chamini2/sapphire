{-# LANGUAGE GADTs #-}
module Checker where

import           Language
import           SymbolTable

import           Control.Arrow          ((&&&))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS
import           Data.Foldable as DF    (mapM_)
import           Data.List              (sortBy)
import           Data.Sequence          (empty)
import           Prelude                hiding (lookup)

type Checker a = RWST CheckReader CheckWriter CheckState Identity a

type CheckReader = [Flag]
data Flag = OutputFile String | SupressWarnings

--------------------------------------------------------------------------------

type CheckWriter = [CheckError]

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
    AlreadyDeclared        :: Identifier -> Position -> StaticError
    -- Operators
    BinaryTypes :: Binary -> [DataType] -> StaticError
    UnaryTypes  :: Unary  -> DataType   -> StaticError
    -- General
    StaticError :: String -> StaticError

instance Show StaticError where
    show (VariableNotDeclared var)     = "Static error: variable '" ++ var ++ "' has not been declared"
    show (VariableNotInitialized var)  = "Static error: variable '" ++ var ++ "' has not been initialized"
    show (InvalidAssignType var vt et) = "Static error: cant assign expression of type '" ++ show et ++ "' to variable '" ++ var ++ "' of type '" ++ show vt ++ "'"
    show (AlreadyDeclared var p)   = "Static error: variable '" ++ var ++ "' has already been declared at " ++ show p
    show (UnaryTypes op dt)   = "Static error: operator '" ++ show op ++ "' doesn't work with arguments '" ++ show dt ++ "'"
    show (BinaryTypes op (dl:dr:[])) = "Static error: operator '" ++ show op ++ "' doesn't work with arguments '(" ++ show dl ++ ", " ++ show dr ++ ")'" -- ++ take 2 (concatMap (\dt -> ", '" ++ show dt) dts) ++ "'"
    show (StaticError msg)    = "Static error: " ++ msg

--------------------------------------------------------------------------------

data CheckState = CheckState
    { table    :: SymTable
    , stack    :: Stack Scope
    , currtSc  :: ScopeNum
    , ast      :: Program
    , currPosn :: Position
    } deriving (Show)

--------------------------------------------------------------------------------

flags :: CheckReader
flags = []

initialState :: CheckState
initialState = CheckState
    { table    = emptyTable
    , stack    = emptyStack
    , currtSc  = 0
    , ast      = Program empty
    , currPosn = (1,1)
    }

----------------------------------------

runProgramChecker :: Checker a -> (CheckState, CheckWriter)
runProgramChecker = (\(_,s,w) -> (s,w)) . runChecker

runChecker :: Checker a -> (a, CheckState, CheckWriter)
runChecker = runIdentity . flip (`runRWST` flags) initialState

getWriter :: Checker a -> CheckWriter
getWriter = (\(_,_,w) -> w) . runChecker

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
            (LError _ _) -> True
            _            -> False
        funcPError e = case e of
            (PError _ _) -> True
            _            -> False
        funcSError e = case e of
            (SError _ _) -> True
            _            -> False

--------------------------------------------------------------------------------

{- |
    Entering a new scope
-}
enterScope :: Checker ()
enterScope = do
    cs <- gets currtSc
    let sc    = Scope { serial = cs + 1 }
        newCs = cs + 1
    modify (\s -> s { stack = push sc (stack s), currtSc = cs + 1 })

{- |
    Exiting a scope that has just been checked
-}
exitScope :: Checker ()
exitScope = modify (\s -> s { stack = snd . pop $ stack s })

{- |
    Adds a symbol to the Checker's symbol table
-}
addSymbol :: Identifier -> SymInfo -> Checker ()
addSymbol var info = modify (\s -> s { table = insert var info (table s)})

{- |
    Gets a symbol's value if it has one or Nothing otherwise
-}
getSymInfoArg :: Identifier -> (SymInfo -> a) -> Checker (Maybe a)
getSymInfoArg var f = do
    tab <- gets table
    maybe failure success $ lookup var tab
    where
        success = return . Just . f
        failure = do
            posn <- gets currPosn
            tell [SError posn $ VariableNotDeclared var]
            return Nothing

{- |
    Changes the value of the variable var to the value val in the symbol table
-}
modifyValue :: Identifier -> Value -> Checker ()
modifyValue var val = do
    tab <- gets table
    let mSym = lookup var tab
    case mSym of
        Just _  -> modify $ func tab
        Nothing -> do
            posn <- gets currPosn
            tell [SError posn $ VariableNotDeclared var]
    where
        func tab s = s { table = update var modValue tab }
        modValue sy = sy { value = Just val }

{- |
    Marks the variable var as initialized.
-}
markInitialized :: Identifier -> Checker ()
markInitialized var = modify $ \s -> s { table = update var (\sym -> sym { initialized = True }) (table s) }


{- |
    Marcamos a la variable var como bloqueada en la tabla de simbolos
-}
--marcarVariableOcupada :: Identifier -> Checker ()
--marcarVariableOcupada var = do
--    tabla <- gets tabla
--    case buscarInfoSim var tabla of
--        Just info -> do
--            eliminarDeclaracion $ Decl var (tipo info)
--            agregarSimbolo var $ info { ocupado = True }
--        otherwise -> continue

----------------------------------------

{- |
    Adds the declaration of var to the symbol table
-}
processDeclaration :: Declaration -> Checker ()
processDeclaration (Declaration var t c) = do
    posn <- gets currPosn
    tab  <- gets table
    cs   <- gets currtSc
    let info = emptySymInfo {
                 dataType = t,
                 category = c,
                 scopeNum = cs,
                 declPosn = posn
               }
    case lookup var tab of
        Nothing -> addSymbol var info
        Just (SymInfo _ _ _ sn op _)
            | sn == cs  -> tell [SError posn $ AlreadyDeclared var op ]
            | otherwise -> addSymbol var info

----------------------------------------

{- |
    Checks the validity of each statement of the program, modifying the state.
    Filling up the symbol table while performing type checking
-}
checkProgram :: Program -> Checker ()
checkProgram pr@(Program sts) = do
    modify (\s -> s {ast = pr})
    DF.mapM_ checkStatement sts

{- |
    Checks the validity of a statement, modifying the state.
-}
checkStatement :: Statement -> Checker () -- Capaz debería devolver otra cosa?
checkStatement StNoop              = return ()
checkStatement (StAssign var ex)   = do
    mayVarDt <- getSymInfoArg var dataType
    expDt    <- checkExpression ex
    case mayVarDt of
        Just varDt -> do
            markInitialized var
            unless (varDt == expDt) $ gets currPosn >>=
                \pos -> tell [SError pos $ InvalidAssignType var varDt expDt]
        Nothing -> return ()
checkStatement (StDeclaration ds)  = DF.mapM_ processDeclaration ds
checkStatement (StReturn ex)       = undefined ex
checkStatement (StRead vars)       = undefined vars
checkStatement (StPrint exs)       = undefined exs
checkStatement (StIf cnd tr el)    = undefined cnd tr el
checkStatement (StCase ex cs def)  = undefined ex cs def
checkStatement (StWhile cnd sts)   = undefined cnd sts
checkStatement (StFor var rng sts) = undefined var rng sts
checkStatement StBreak             = return ()
checkStatement StContinue          = return ()

{- |
    Checks the validity of an expression and returns it's value.
-}
checkExpression :: Expression -> Checker DataType
checkExpression (Variable var)   = do
    mayInf <- getSymInfoArg var (initialized &&& dataType) -- (\si -> (initialized si, dataType si))
    case mayInf of
        Just (ini, dt) -> do
            unless ini $ gets currPosn >>=
                \pos -> tell [SError pos $ VariableNotInitialized var]
            return dt
        Nothing         -> return Void
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
