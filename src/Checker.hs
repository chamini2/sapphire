module Checker where

import           Language
import           SymbolTable

import           Control.Arrow          ((&&&), second)
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS
import           Data.Foldable as DF    (mapM_, elem, toList)
import           Data.Function          (on)
import           Data.List              (sortBy, find)
import           Data.Sequence as DS    (empty, singleton, Seq, (><))
import           Prelude       as P     hiding (lookup, zipWith, elem)

type Checker a = RWST CheckReader CheckWriter CheckState Identity a

type CheckReader = [Flag]
data Flag = OutputFile String | SupressWarnings

--------------------------------------------------------------------------------

type CheckWriter = [CheckError]

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
    show (UnexpectedChar c) = "unexpected character'" ++ [c] ++ "'"
    show (StringError str)  = "missing matching '\"' for string '" ++ str ++ "'"
    show (LexerError  msg)  = msg

data ParseError
    = UnexpectedToken String{-Token-}
    | ParseError      String

instance Show ParseError where
    show (UnexpectedToken tok) = "unexpected token: '" ++ show tok ++ "'"
    show (ParseError msg)      = msg

data StaticError
    -- Variables
    = VariableNotDeclared    Identifier
    | VariableNotInitialized Identifier
    | InvalidAssignType      Identifier DataType DataType
    | AlreadyDeclared        Identifier Position
    -- Statements
    | IfConditionDataType    DataType
    -- Operators
    | BinaryTypes Binary (DataType, DataType)
    | UnaryTypes  Unary  DataType
    -- General
    | StaticError String

instance Show StaticError where
    -- Variables
    show (VariableNotDeclared var)     = "variable '" ++ var ++ "' has not been declared"
    show (VariableNotInitialized var)  = "variable '" ++ var ++ "' has not been initialized"
    show (InvalidAssignType var vt et) = "cant assign expression of type '" ++ show et ++ "' to variable '" ++ var ++ "' of type '" ++ show vt ++ "'"
    show (AlreadyDeclared var p)       = "variable '" ++ var ++ "' has already been declared at " ++ show p
    -- Statements
    show (IfConditionDataType dt)      = "condition of if statement must be of type 'Bool', but is of type '" ++ show dt ++ "'"
    -- Operators
    show (UnaryTypes op dt)            = "operator '" ++ show op ++ "' doesn't work with arguments '" ++ show dt ++ "'"
    show (BinaryTypes op (dl,dr))      = "operator '" ++ show op ++ "' doesn't work with arguments '(" ++ show dl ++ ", " ++ show dr ++ ")'" -- ++ take 2 (concatMap (\dt -> ", '" ++ show dt) dts) ++ "'"
    -- General
    show (StaticError msg)             = msg

--------------------------------------------------------------------------------

data CheckState = CheckState
    { table    :: SymTable
    , stack    :: Stack Scope
    , currtSc  :: ScopeNum
    , ast      :: Program
    }

instance Show CheckState where
    show (CheckState t s c a) = showT ++ showS ++ showC ++ showA
        where
            showT = "Symbol Table:\n" ++ show t ++ "\n"
            showS = "Scope Stack:\n"  ++ show s ++ "\n"
            showC = "Scope Number:\t" ++ show c ++ "\n"
            showA = "Program:\n"      ++ show a ++ "\n"

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
    Gets a symbol's value said value exists in the table, otherwise Nothing
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
markInitialized :: Identifier -> Checker ()
markInitialized var = modify $ \s -> s { table = update var (\sym -> sym { initialized = True }) (table s) }

{- |
    Returns the variables in the current scope
-}
getScopeVariables :: Checker [(Identifier, SymInfo)]
getScopeVariables = do
    vars <- gets $ accessible . table
    stc <- gets stack
    let stcS = fmap serial stc
    return $ foldr (func stcS) [] . fmap (second DF.toList) $ DF.toList vars
    where
        --func stc (_, info) = scopeNum info `elem` stc
        func stc a b = case funcFind stc a of
            (v, Just x)  -> (v,x) : b
            (_, Nothing) -> b
        funcFind stc (v, infos) = (v, find (\i -> scopeNum i `elem` stc) infos)

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
    StNoop -> return ()

    StAssign varL@(Lex var _) ex -> do
        mayVarDt <- getSymInfoArg varL dataType
        expDt    <- checkExpression ex
        case mayVarDt of
            Just varDt -> do
                markInitialized var
                unless (varDt == expDt) $
                    tell [SError posn $ InvalidAssignType var varDt expDt]
            Nothing -> return ()

    StDeclaration ds      -> DF.mapM_ processDeclaration ds

    StReturn ex           -> return ()

    StFunctionDef dcl dts -> return ()

    StFunctionImp iden parms body -> return ()

    StFunctionCall iden args -> return ()

    StRead vars -> return ()

    StPrint exs -> DF.mapM_ checkExpression exs

    StIf cnd success failure -> do
        dt <- checkExpression cnd
        case dt of
            Bool -> do
                before      <- getScopeVariables
                stateBefore <- get

                checkStatements success
                varSucc <- getScopeVariables

                -- Reiniciando el estado
                stateSucc <- get
                put stateBefore

                checkStatements failure
                varFail <- getScopeVariables

                stateFail <- get
                put stateBefore

                --let after = zipWith3 zipFunc varSucc varFail before
                --let after = foldr func [] $ sortBy (\(a,_) (b,_) -> compare a b) $ varSucc ++ varFail
                    --final = foldr (func (||)) [] $ sortBy (\(a,_) (b,_) -> compare a b) $ before  ++ after

                --forM_ after $
                --    \(var,info) -> when (initialized info) $ markInitialized var

                --tell [SError posn $ StaticError ("before:" ++ concatMap (("\n\t\t"++) . show) before)]
                --tell [SError posn $ StaticError ("varSucc:" ++ concatMap (("\n\t\t"++) . show) varSucc)]
                --tell [SError posn $ StaticError ("varFail:" ++ concatMap (("\n\t\t"++) . show) varFail)]
                --tell [SError posn $ StaticError ("after:" ++ concatMap (("\n\t\t"++) . show) after)]
                --tell [SError posn $ StaticError "---------------------------"]
            _    -> tell [SError posn $ IfConditionDataType dt]
        where
            -- before || (varSucc && varFail) == initialized
            --zipFunc (_,sI) (_,fI) (var,bI) = (var, bI { initialized =
            --        initialized bI || initialized sI && initialized fI })
            func a@(aVar,aInfo) bs = case bs of
                [] -> [a]
                ((bVar, bInfo) : tbs) ->
                    case (aVar == bVar, initialized aInfo, initialized bInfo) of
                        (True,True,True) -> (aVar, aInfo { initialized = True  }) : tbs
                        (False,_,_)      -> a : bs
                        _                -> (aVar, aInfo { initialized = False }) : tbs

    StCase ex cs def -> return ()

    StLoop rep cnd sts -> return ()

    StFor var rng sts -> return ()

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

--------------------------------------------------------------------------------

getExpressionVariables :: Lexeme Expression -> Seq (Lexeme Identifier)
getExpressionVariables (Lex e _) = case e of
    Variable var     -> singleton var
    ExpBinary _ l r  -> getExpressionVariables l >< getExpressionVariables r
    ExpUnary _ expr  -> getExpressionVariables expr
    -- (LitInt _) (LitFloat _) (LitBool _) (LitChar _) (LitString _)
    _                -> empty
