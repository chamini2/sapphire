module Checker where

import           Language
import           SymbolTable

import           Control.Arrow          ((&&&))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS
import           Data.Foldable as DF    (mapM_)
import           Data.Function          (on)
import           Data.List              (sortBy)
import           Data.Sequence as DS    (empty, Seq)
import           Prelude                hiding (lookup)

type Checker a = RWST CheckReader CheckWriter CheckState Identity a

type CheckReader = [Flag]
data Flag = OutputFile String | SupressWarnings

--------------------------------------------------------------------------------

type CheckWriter = [CheckError]

data CheckError
    = LError Position LexerError
    | PError Position ParseError
    | SError Position StaticError

errorPos :: CheckError -> Position
errorPos (LError p _) = p
errorPos (PError p _) = p
errorPos (SError p _) = p

instance Show CheckError where
    show (LError p e) = "error on " ++ showPosn p ++ "\n\t" ++ show e ++ "\n"
    show (PError p e) = "error on " ++ showPosn p ++ "\n\t" ++ show e ++ "\n"
    show (SError p e) = "error on " ++ showPosn p ++ "\n\t" ++ show e ++ "\n"

instance Eq CheckError where
    (==) = (==) `on` errorPos

instance Ord CheckError where
    compare = compare `on` errorPos

data LexerError
    = UnexpectedChar Char
    | StringError    String
    | LexerError     String

instance Show LexerError where
    show (UnexpectedChar c) = "Unexpected character: '" ++ [c] ++ "'"
    show (StringError str)  = "Missing matching '\"' for string '" ++ str ++ "'"
    show (LexerError  msg)  = "Lexing error: " ++ msg

data ParseError
    = UnexpectedToken String{-Token-}
    | ParseError      String

instance Show ParseError where
    show (UnexpectedToken tok) = "Parsing error on Token: " ++ show tok
    show (ParseError msg)      = "Parsing error: " ++ msg

data StaticError
    -- Variables
    = VariableNotDeclared    Identifier
    | VariableNotInitialized Identifier
    | InvalidAssignType      Identifier DataType DataType
    | AlreadyDeclared        Identifier Position
    -- Operators
    | BinaryTypes Binary (DataType, DataType)
    | UnaryTypes  Unary  DataType
    -- General
    | StaticError String

instance Show StaticError where
    show (VariableNotDeclared var)     = "Static error: variable '" ++ var ++ "' has not been declared"
    show (VariableNotInitialized var)  = "Static error: variable '" ++ var ++ "' has not been initialized"
    show (InvalidAssignType var vt et) = "Static error: cant assign expression of type '" ++ show et ++ "' to variable '" ++ var ++ "' of type '" ++ show vt ++ "'"
    show (AlreadyDeclared var p)   = "Static error: variable '" ++ var ++ "' has already been declared at " ++ show p
    show (UnaryTypes op dt)   = "Static error: operator '" ++ show op ++ "' doesn't work with arguments '" ++ show dt ++ "'"
    show (BinaryTypes op (dl,dr)) = "Static error: operator '" ++ show op ++ "' doesn't work with arguments '(" ++ show dl ++ ", " ++ show dr ++ ")'" -- ++ take 2 (concatMap (\dt -> ", '" ++ show dt) dts) ++ "'"
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
    , stack    = initialStack
    , currtSc  = 0
    , ast      = Program DS.empty
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

getErrors :: CheckWriter -> ([CheckError], [CheckError], [CheckError])
getErrors errors = (only lexical, only parsing, only static)
    where
        only f = sortBy compare $ filter f errors
        lexical e = case e of
            (LError _ _) -> True
            _            -> False
        parsing e = case e of
            (PError _ _) -> True
            _            -> False
        static e = case e of
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
    modify (\s -> s { stack = push sc (stack s), currtSc = cs + 1 })

{- |
    Exiting a scope that has just been checked
-}
exitScope :: Checker ()
exitScope = modify (\s -> s { stack = snd . pop $ stack s })

{- |
    Adds a symbol to the Checker's symbol table
-}
addSymbol :: Lexeme Identifier -> SymInfo -> Checker ()
addSymbol (Lex var _) info = modify func
    where
        func s = s { table = insert var info (table s)}
{- |
    Gets a symbol's value if it has one or Nothing otherwise
-}
getSymInfoArg :: Lexeme Identifier -> (SymInfo -> a) -> Checker (Maybe a)
getSymInfoArg (Lex var posn) f = do
    tab <- gets table
    maybe failure success $ lookup var tab
    where
        success = return . Just . f
        failure = do
            tell [SError posn $ VariableNotDeclared var]
            return Nothing

{- |
    Changes the value of the variable var to the value val in the symbol table
-}
modifyValue :: Lexeme Identifier -> Value -> Checker ()
modifyValue (Lex var posn) val = do
    tab <- gets table
    let mSym = lookup var tab
    case mSym of
        Just _  -> modify $ func tab
        Nothing -> tell [SError posn $ VariableNotDeclared var]
    where
        func tab s = s { table = update var modValue tab }
        modValue sy = sy { value = Just val }

{- |
    Marks the variable var as initialized.
-}
markInitialized :: Lexeme Identifier -> Checker ()
markInitialized (Lex var _) = modify $ \s -> s { table = update var (\sym -> sym { initialized = True }) (table s) }

----------------------------------------

{- |
    Adds the declaration of var to the symbol table
-}
processDeclaration :: Lexeme Declaration -> Checker ()
processDeclaration (Lex (Declaration varL@(Lex var _) (Lex t _) c) posn) = do
    tab  <- gets table
    cs   <- gets currtSc
    let info = emptySymInfo {
                 dataType = t,
                 category = c,
                 scopeNum = cs,
                 declPosn = posn
               }
    case lookup var tab of
        Nothing -> addSymbol varL info
        Just (SymInfo _ _ _ sn op _)
            | sn == cs  -> tell [SError posn $ AlreadyDeclared var op ]
            | otherwise -> addSymbol varL info

----------------------------------------

{- |
    Checks the validity of each statement of the program, modifying the state.
-}
checkProgram :: Program -> Checker ()
checkProgram pr@(Program sts) = do
    modify (\s -> s {ast = pr})
    checkStatements sts

checkStatements :: Seq (Lexeme Statement) -> Checker ()
checkStatements = DF.mapM_ checkStatement

{- |
    Checks the validity of a statement, modifying the state.
-}
checkStatement :: Lexeme Statement -> Checker () -- Capaz debería devolver otra cosa?
checkStatement (Lex st posn) = case st of
    StNoop                       -> return ()
    StAssign varL@(Lex var _) ex -> do
        mayVarDt <- getSymInfoArg varL dataType
        expDt    <- checkExpression ex
        case mayVarDt of
            Just varDt -> do
                markInitialized varL
                unless (varDt == expDt) $
                    tell [SError posn $ InvalidAssignType var varDt expDt]
            Nothing -> return ()
    StDeclaration ds             -> DF.mapM_ processDeclaration ds
    StReturn ex                  ->  undefined ex
    StRead vars                  ->  undefined vars
    StPrint exs                  ->  DF.mapM_ checkExpression exs
    StIf cnd tr el               -> undefined cnd tr el --do
        --_ <- checkExpression cnd
        --checkStatements tr
        --checkStatements el
    StCase ex cs def             -> undefined ex cs def
    StWhile cnd sts              -> undefined cnd sts
    StFor var rng sts            -> undefined var rng sts
    StBreak -> return ()
    StContinue -> return ()

{- |
    Checks the validity of an expression and returns it's value.
-}
checkExpression :: Lexeme Expression -> Checker DataType
checkExpression (Lex e posn) = case e of
    Variable varL@(Lex var _) -> do
        mayInf <- getSymInfoArg varL (initialized &&& dataType) -- (\si -> (initialized si, dataType si))
        case mayInf of
            Just (ini, dt) -> do
                unless ini $ tell [SError posn $ VariableNotInitialized var]
                return dt
            Nothing -> return Void
    LitInt _                  -> return Int
    LitFloat _                -> return Float
    LitBool _                 -> return Bool
    LitChar _                 -> return Char
    LitString _               -> return String
    ExpBinary (Lex op _) l r  -> do
        ldt <- checkExpression l
        rdt <- checkExpression r
        let dts = filter (((ldt,rdt)==) . fst) $ binaryOperation op
        if null dts
            then do
                tell [SError posn $ BinaryTypes op (ldt,rdt)]
                return . snd . head $ binaryOperation op
            else return . snd $ head dts
    ExpUnary (Lex op _) expr  -> do
        dt   <- checkExpression expr
        let dts = filter ((dt==) . fst) $ unaryOperation op
        if null dts
            then do
                tell [SError posn $ UnaryTypes op dt]
                return . snd . head $ unaryOperation op
            else return . snd $ head dts
