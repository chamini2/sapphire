module Checker where

import           Language
import           SymbolTable

import           Control.Arrow          (second, (&&&))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS      hiding (forM, forM_, mapM_)
import           Data.Foldable          as DF (elem, foldr, forM_, mapM_,
                                               toList)
import           Data.Function          (on)
import           Data.Functor           ((<$))
import           Data.List              (find, sortBy)
import           Data.Sequence          as DS (Seq, empty, fromList, index,
                                               singleton, zipWith, (<|), (|>))
import           Data.Traversable       as DT (forM)
import           Prelude                as P hiding (elem, foldr, lookup, mapM_,
                                              zipWith)

type Checker a = RWST CheckReader CheckWriter CheckState Identity a

type CheckReader = [Flag]
data Flag = OutputFile String | SupressWarnings

--------------------------------------------------------------------------------

type CheckWriter = Seq CheckError

data CheckError
    = LError Position LexerError
    | PError Position ParseError
    | SError Position StaticError

instance Show CheckError where
    show (LError p e) = "Lexer error on "   ++ showPosn p ++ "\n\t" ++ show e ++ "\n"
    show (PError p e) = "Parsing error on " ++ showPosn p ++ "\n\t" ++ show e ++ "\n"
    show (SError p e) = "Static error on "  ++ showPosn p ++ "\n\t" ++ show e ++ "\n"

instance Eq CheckError where
    (==) = (==) `on` errorPos

instance Ord CheckError where
    compare = compare `on` errorPos

data LexerError
    = UnexpectedChar Char
    | StringError    String
    | LexerError     String

instance Show LexerError where
    show (UnexpectedChar c) = "unexpected character '" ++ [c] ++ "'"
    show (StringError str)  = "missing matching \" for string " ++ show str
    show (LexerError  msg)  = msg

data ParseError
    = UnexpectedToken String -- show Token
    | ParseError      String

instance Show ParseError where
    show (UnexpectedToken tok) = "unexpected token: '" ++ show tok ++ "'"
    show (ParseError msg)      = msg

data StaticError
    -- Variables
    = VariableNotDeclared    Identifier
    | VariableNotInitialized Identifier
    | InvalidAssignType      Identifier DataType DataType
    -- Functions
    | FunctionNotDefined     Identifier
    | FunctionNotDeclared    Identifier
    -- Statements
    | ConditionDataType DataType
    | CaseWhenDataType  DataType DataType
    | ForInDataType     DataType
    | BreakOutsideLoop
    | ContinueOutsideLoop
    -- Operators
    | BinaryTypes Binary (DataType, DataType)
    | UnaryTypes  Unary  DataType
    -- General
    | StaticError String
    | AlreadyDeclared        Identifier Position

instance Show StaticError where
    -- Variables
    show (VariableNotDeclared var)     = "variable '" ++ var ++ "' has not been declared"
    show (VariableNotInitialized var)  = "variable '" ++ var ++ "' has not been initialized"
    show (InvalidAssignType var vt et) = "cant assign expression of type '" ++ show et ++ "' to variable '" ++ var ++ "' of type '" ++ show vt ++ "'"
    -- Functions
    show (FunctionNotDeclared fname)   = "function '" ++ fname ++ "' has not been declared"
    show (FunctionNotDefined fname)    = "must define function '" ++ fname ++ "' before implementing it"
    -- Statements
    show (ConditionDataType dt)        = "condition must be of type 'Bool', but it is of type '" ++ show dt ++ "'"
    show (CaseWhenDataType dt wd)      = "case has expression of type '" ++ show dt ++ "' but when has expression of type '" ++ show wd ++ "'"
    show (ForInDataType dt)            = "for statement must iterate over expression of type 'Range', but it is of type '" ++ show dt ++ "'"
    show BreakOutsideLoop              = "break statement not within loop"
    show ContinueOutsideLoop           = "continue statement not within loop"
    -- Operators
    show (UnaryTypes op dt)            = "operator '" ++ show op ++ "' doesn't work with arguments '" ++ show dt ++ "'"
    show (BinaryTypes op (dl,dr))      = "operator '" ++ show op ++ "' doesn't work with arguments '(" ++ show dl ++ ", " ++ show dr ++ ")'"
    -- General
    show (StaticError msg)             = msg
    show (AlreadyDeclared var p)       = "identifier '" ++ var ++ "' has already been declared at " ++ show p

--------------------------------------------------------------------------------

data CheckState = CheckState
    { table   :: SymTable
    , stack   :: Stack Scope
    , currtSc :: ScopeNum
    , ast     :: Program
    , loop    :: LoopLevel
    }

instance Show CheckState where
    show (CheckState t s c a _) = showT ++ showS ++ showC ++ showA
        where
            showT = "Symbol Table:\n" ++ show t ++ "\n"
            showS = "Scope Stack:\n"  ++ show s ++ "\n"
            showC = "Scope Number:\t" ++ show c ++ "\n"
            showA = "Program:\n"      ++ show a ++ "\n"

type LoopLevel = Int

--------------------------------------------------------------------------------

errorPos :: CheckError -> Position
errorPos (LError p _) = p
errorPos (PError p _) = p
errorPos (SError p _) = p

flags :: CheckReader
flags = []

initialState :: CheckState
initialState = CheckState
    { table    = emptyTable
    , stack    = initialStack
    , currtSc  = 0
    , ast      = Program DS.empty
    , loop     = 0
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
        only f = sortBy compare . filter f $ toList errors
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
    let sc = Scope { serial = cs + 1 }
    modify (\s -> s { stack = push sc (stack s), currtSc = cs + 1 })

{- |
    Exiting a scope that has just been checked
-}
exitScope :: Checker ()
exitScope = modify (\s -> s { stack = snd . pop $ stack s })

{- |
    Entering a loop
-}
enterLoop :: Checker ()
enterLoop = modify (\s -> s { loop = loop s + 1 })

{- |
    Exiting a loop
-}
exitLoop :: Checker ()
exitLoop = modify (\s -> s { loop = loop s - 1 })


{- |
    Adds a symbol to the Checker's symbol table
-}
addSymbol :: Lexeme Identifier -> SymInfo -> Checker ()
addSymbol (Lex var _) info = modify $ \s -> s { table = insert var info (table s)}

{- |
    Gets a symbol's attribute if said symbol exists in the table, otherwise Nothing
-}
getsSymInfo :: Lexeme Identifier -> (SymInfo -> a) -> Checker (Maybe a)
getsSymInfo (Lex iden posn) f = do
    (tab, stck) <- gets (table &&& stack)
    maybe failure success $ lookupWithScope iden stck tab
    where
        success = return . Just . f
        failure = tell (singleton $ SError posn $ VariableNotDeclared iden) >> return Nothing

{- |
    Modifies a symbol if said symbol exists in the table
-}
modifySymInfo :: Lexeme Identifier -> (SymInfo -> SymInfo) -> Checker ()
modifySymInfo var@(Lex iden _) f = do
    tab <- gets table
    maySi <- getsSymInfo var id
    case maySi of
        Just si -> modify (\s -> s { table = updateWithScope iden (scopeNum si) f tab })
        Nothing -> return ()

{- |
    Changes the value of the variable to the value specified in the symbol table
-}
putValue :: Lexeme Identifier -> Value -> Checker ()
putValue var val = modifySymInfo var (\sym -> sym { value = Just val })

{- |
    Marks the specifued variable as initialized
-}
markInitialized :: Lexeme Identifier -> Checker ()
markInitialized var = modifySymInfo var (\sym -> sym { initialized = True })

{- |
    Returns the variables in the current scope
-}
getScopeVariables :: Checker (Seq (Identifier, SymInfo))
getScopeVariables = do
    vars <- gets $ accessible . table
    stck <- gets stack
    let stckS = fmap serial stck
    return $ foldr (func stckS) empty . fmap (second toList) $ toList vars
    where
        func stc a b = case funcFind stc a of
            (v,Just x)  -> (v,x) <| b
            (_,Nothing) -> b
        funcFind stc (v, infos) = (v, find (\i -> scopeNum i `elem` stc) infos)

{- |
    Replaces the variables in the current scope
-}
putScopeVariables :: Seq (Identifier, SymInfo) -> Checker ()
putScopeVariables vars = forM_ vars $ \(var, si) -> do
    tab <- gets table
    let newTab = updateWithScope var (scopeNum si) (const si) tab
    modify (\s -> s { table = newTab })


checkInitialization :: Seq (Seq (Identifier, SymInfo)) -> Seq (Identifier, SymInfo)
checkInitialization scopes = foldr (zipWith func) (index scopes 0) scopes
    where
        func :: (Identifier, SymInfo) -> (Identifier, SymInfo) -> (Identifier, SymInfo)
        func (a,sa) (b,sb) = if a /= b
            then error "Checker.checkInitialization: zipping different variabls"
            else (a, sa { initialized = initialized sa && initialized sb })

----------------------------------------

{- |
    Adds the declaration of var to the symbol table
-}
processDeclaration :: Lexeme Declaration -> Checker ()
processDeclaration (Lex (Declaration varL@(Lex var _) (Lex t _) c) posn) = do
    tab <- gets table
    cs  <- gets currtSc
    let info = emptySymInfo {
                 dataType = t,
                 category = c,
                 scopeNum = cs,
                 declPosn = posn
               }
    case lookup var tab of
        Nothing -> addSymbol varL info
        Just si
            | scopeNum si == cs  -> tell (singleton $ SError posn $ AlreadyDeclared var (declPosn si))
            | otherwise          -> addSymbol varL info

----------------------------------------

{- |
    Checks the validity of each statement of the program, modifying the state.
-}
checkProgram :: Seq CheckError -> Program -> Checker ()
checkProgram lexErrors pr@(Program sts) = do
    modify (\s -> s { ast = pr })
    tell lexErrors
    checkStatements sts

checkStatements :: StBlock -> Checker ()
checkStatements = mapM_ checkStatement

{- |
    Checks the validity of a statement, modifying the state.
-}
checkStatement :: Lexeme Statement -> Checker () -- Capaz debería devolver otra cosa?
checkStatement (Lex st posn) = case st of
    StNoop -> return ()

    StAssign varL@(Lex var _) ex -> do
        mayVarDt <- getsSymInfo varL dataType
        expDt    <- checkExpression ex
        case mayVarDt of
            Just varDt -> do
                markInitialized varL
                unless (varDt == expDt) $
                    tell (singleton $ SError posn $ InvalidAssignType var varDt expDt)
            Nothing -> return ()

    StDeclaration dcl -> processDeclaration dcl

    StReturn ex       -> void $ checkExpression ex

    StFunctionDef decl@(Lex (Declaration iden _ _) _) dts -> do
        processDeclaration decl
        putValue iden $ ValFunction dts empty

    --StFunctionImp fname@(Lex iden _) params body -> do
    StFunctionImp fname@(Lex iden _) _ body -> do
        val <- getsSymInfo fname value
        maybe failure success val
        where failure = tell (singleton $ SError posn $ FunctionNotDefined iden)
              success = maybe failure func
              func v  = putValue fname $ ValFunction (parameters v) body

    --StFunctionCall fname args -> return ()
    StFunctionCall _ _ -> return ()

    StRead vars -> forM_ vars $ \varL -> do
        mayInf <- getsSymInfo varL dataType
        case mayInf of
            --Just dt -> markInitialized varL
            Just _ -> markInitialized varL
            Nothing -> return ()

    StPrint exs -> mapM_ checkExpression exs

    StIf cnd success failure -> do
        checkExpression cnd >>= \dt -> unless (dt == Bool) $ tell (singleton $ SError posn $ ConditionDataType dt)

        before <- getScopeVariables

        enterScope
        checkStatements success
        exitScope
        varSucc <- getScopeVariables

        putScopeVariables before

        enterScope
        checkStatements failure
        exitScope
        varFail <- getScopeVariables

        putScopeVariables before

        forM_ (checkInitialization $ fromList [varSucc,varFail]) $
            \(var,info) -> when (initialized info) $ markInitialized (Lex var (declPosn info))

    StCase ex cs els -> do
        dt <- checkExpression ex
        before <- getScopeVariables

        varScopes <- forM cs $ \(Lex (When wexps sts) wposn) -> do
            --forM_ wexps $ \wexp -> checkExpression wexp >>= \wd ->
            forM_ wexps $ checkExpression >=> \wd ->
                when (wd /= dt) $ tell (singleton $ SError wposn $ CaseWhenDataType dt wd)

            enterScope
            checkStatements sts
            exitScope
            getScopeVariables

        enterScope
        checkStatements els
        exitScope
        varOtherwise <- getScopeVariables

        putScopeVariables before
        forM_ (checkInitialization $ varScopes |> varOtherwise) $
            \(var,info) -> when (initialized info) $ markInitialized (Lex var (declPosn info))

    StLoop rep cnd body -> do
        enterScope >> enterLoop
        checkStatements rep
        exitLoop >> exitScope

        checkExpression cnd >>= \dt -> unless (dt == Bool) $ tell (singleton $ SError posn $ ConditionDataType dt)

        before <- getScopeVariables

        enterScope >> enterLoop
        checkStatements body
        exitLoop >> exitScope

        putScopeVariables before

    StFor var rng body -> do
        dt <- checkExpression rng
        unless (dt == Range) $ tell (singleton $ SError posn $ ForInDataType dt)

        before <- getScopeVariables

        enterScope >> enterLoop
        processDeclaration $ Declaration var (Int <$ var) CatVariable <$ var
        markInitialized var
        checkStatements body
        exitLoop >> exitScope

        putScopeVariables before

    StBreak -> do
        loopL <- gets loop
        unless (loopL > 0) $ tell (singleton $ SError posn BreakOutsideLoop)

    StContinue -> do
        loopL <- gets loop
        unless (loopL > 0) $ tell (singleton $ SError posn ContinueOutsideLoop)

{- |
    Checks the validity of an expression and returns it's value.
-}
checkExpression :: Lexeme Expression -> Checker DataType
checkExpression (Lex e posn) = case e of
    Variable varL@(Lex var _) -> do
        mayInf <- getsSymInfo varL (initialized &&& dataType)       -- (\si -> (initialized si, dataType si))
        case mayInf of
            Just (ini, dt) -> do
                unless ini $ tell (singleton $ SError posn $ VariableNotInitialized var)
                return dt
            Nothing -> return Void

    --FunctionCall iden exs -> undefined
    FunctionCall _ _ -> undefined

    LitInt _    -> return Int

    LitFloat _  -> return Float

    LitBool _   -> return Bool

    LitChar _   -> return Char

    LitString _ -> return String

    ExpBinary (Lex op _) l r  -> do
        ldt <- checkExpression l
        rdt <- checkExpression r
        let dts = filter (((ldt,rdt)==) . fst) $ binaryOperation op
        if null dts
            then do
                tell (singleton $ SError posn $ BinaryTypes op (ldt,rdt))
                return . snd . head $ binaryOperation op
            else return . snd $ head dts

    ExpUnary (Lex op _) expr  -> do
        dt   <- checkExpression expr
        let dts = filter ((dt==) . fst) $ unaryOperation op
        if null dts
            then do
                tell (singleton $ SError posn $ UnaryTypes op dt)
                return . snd . head $ unaryOperation op
            else return . snd $ head dts
