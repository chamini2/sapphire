{-# LANGUAGE ScopedTypeVariables #-}
module Checker where

import           Language
import           SymbolTable

import           Control.Arrow          ((&&&))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS      hiding (forM, forM_, mapM, mapM_)
import           Data.Foldable          as DF (and, concatMap, elem, find,
                                               foldr, forM_, mapM_, notElem,
                                               toList, foldr1, foldlM, foldl)
import           Data.Function          (on)
import           Data.Functor           ((<$), (<$>))
import           Data.List              (intercalate)
import qualified Data.Map               as DM (empty, insertLookupWithKey, Map)
import           Data.Maybe             (fromJust, isJust, isNothing)
import           Data.Sequence          as DS (Seq, empty, filter, fromList,
                                               index, length, null, singleton,
                                               sortBy, zip, zipWith, (<|), (|>),
                                               (><), ViewL((:<),EmptyL), viewl)
import           Data.Traversable       as DT (forM, mapM)
import           Prelude                as P hiding (and, concatMap, elem,
                                              filter, foldr, length, lookup,
                                              mapM, mapM_, notElem, null, zip,
                                              zipWith, foldr1, foldl)

type Checker a = RWST CheckReader CheckWriter CheckState Identity a

type CheckReader = Seq Flag
data Flag = OutputFile String | SupressWarnings | AllWarnings
    deriving (Eq)
--------------------------------------------------------------------------------

type CheckWriter = Seq CheckError

data CheckError
    = LError Position LexerError
    | PError Position ParseError
    | SError Position StaticError
    | CWarn  Position CheckWarning

instance Show CheckError where
    show cError = case cError of
        LError p e -> "Lexer error on "   ++ showPosn p ++ ": \n\t" ++ show e ++ "\n"
        PError p e -> "Parsing error on " ++ showPosn p ++ ": \n\t" ++ show e ++ "\n"
        SError p e -> "Static error on "  ++ showPosn p ++ ": \n\t" ++ show e ++ "\n"
        CWarn  p w -> "Warning on "       ++ showPosn p ++ ": \n\t" ++ show w ++ "\n"

instance Eq CheckError where
    (==) = (==) `on` errorPos

instance Ord CheckError where
    compare = compare `on` errorPos

data LexerError
    = UnexpectedChar Char
    | StringError    String
    | LexerError     String

instance Show LexerError where
    show lError = case lError of
        UnexpectedChar c -> "unexpected character '" ++ [c] ++ "'"
        StringError str  -> "missing matching \" for string " ++ show str
        LexerError  msg  -> msg

data ParseError
    = UnexpectedToken String -- show Token
    | ParseError      String

instance Show ParseError where
    show pError = case pError of
        UnexpectedToken tok -> "unexpected token: '" ++ show tok ++ "'"
        ParseError msg      -> msg

data StaticError
    -- Variables
    = VariableNotInitialized Identifier
    | InvalidAssignType      Identifier DataType DataType
    | VariableNonArray       Identifier DataType
    | VariableNonStruct      Identifier DataType
    | StructNoField          Identifier Identifier
    | IndexDataType          Expression DataType
    | ArraySizeDataType      Expression DataType
    -- Types
    | TypeAlreadyDefined   Identifier Position
    | LanguageTypeRedefine Identifier
    | UndefinedType        Identifier
    -- Functions
    | FunctionNotDefined       Identifier
    | ProcedureInExpression    Identifier
    | FunctionAsStatement      Identifier
    | UsedNotImplemented       Identifier
    | ImpInDefScope            Identifier Position
    | AlreadyImplemented       Identifier Position
    | LanguageImplemented      Identifier
    | FunctionArguments        Identifier (Seq DataType) (Seq DataType)
    | FunctionAlreadyDefined   Identifier Position
    | LanguageFunctionRedefine Identifier
    | NoReturn                 Identifier
    -- Statements
    | ConditionDataType DataType
    | CaseWhenDataType  DataType DataType
    | ForInDataType     DataType
    | BreakOutsideLoop
    | ContinueOutsideLoop
    | ReturnProcedure            DataType Identifier
    | ReturnType        DataType DataType Identifier
    -- Operators
    | BinaryTypes Binary (DataType, DataType)
    | UnaryTypes  Unary  DataType
    -- General
    | WrongCategory   Identifier Category Category
    | NotDefined      Identifier
    | AlreadyDeclared Identifier Position
    | StaticError     String

instance Show StaticError where
    show sError = case sError of
        -- Variables
        VariableNotInitialized var       -> "variable '" ++ var ++ "' may not have been initialized"
        InvalidAssignType      var vt et -> "cannot assign expression of type '" ++ show et ++ "' to variable '" ++ var ++ "' of type '" ++ show vt ++ "'"
        VariableNonArray       var dt    -> "variable '" ++ var ++ "' of type '" ++ show dt ++ "' is being used as an array"
        VariableNonStruct      var dt    -> "variable '" ++ var ++ "' of type '" ++ show dt ++ "' is being used as a structure"
        StructNoField          str fn    -> "structure '" ++ str ++ "' has no field named '" ++ fn ++ "'"
        IndexDataType          expr dt   -> "index expression '" ++ showIndex expr ++ "' is of type '" ++ show dt ++ "', but 'Int' was expected"
        ArraySizeDataType      expr dt   -> "array size expression '" ++ showIndex expr ++ "' is of type '" ++ show dt ++ "', but 'Int' was expected"
        -- Types
        TypeAlreadyDefined   tname p -> "type '" ++ tname ++ "' has already been defined at " ++ show p
        LanguageTypeRedefine tname   -> "cannot redefine a language defined type '" ++ tname ++ "'"
        UndefinedType        tname   -> "type '" ++ tname ++ "' has not been defined"
        -- Functions
        FunctionNotDefined    fname     -> "must define function '" ++ fname ++ "' before implementing it"
        ProcedureInExpression fname     -> "cannot use procedure '" ++ fname ++ "' inside an expression"
        FunctionAsStatement   fname     -> "cannot use function '" ++ fname ++ "' as a statement"
        UsedNotImplemented    fname     -> "function '" ++ fname ++ "' is used but never implemented"
        ImpInDefScope         fname p   -> "must implement function '" ++ fname ++ "' in same scope that it is defined, at " ++ showPosn p
        AlreadyImplemented    fname p   -> "function '" ++ fname ++ "' has already been implemented at " ++ showPosn p
        LanguageImplemented   fname     -> "cannot reimplement language implemented function '"++ fname ++ "'"
        FunctionArguments     fname e g -> "function '" ++ fname ++ "' expects arguments (" ++ showSign e ++ "), but was given (" ++ showSign g ++ ")"
            where
                showSign = intercalate ", " . map show . toList
        FunctionAlreadyDefined   fname p -> "function '" ++ fname ++ "' has already been defined at " ++ show p
        LanguageFunctionRedefine fname   -> "cannot redefine a language defined function '" ++ fname ++ "'"
        NoReturn                 fname   -> "function '" ++ fname ++ "' does not have a return statement"
        -- Statements
        ConditionDataType dt    -> "condition must be of type 'Bool', but it is of type '" ++ show dt ++ "'"
        CaseWhenDataType e g    -> "case has expression of type '" ++ show e ++ "' but when has expression of type '" ++ show g ++ "'"
        ForInDataType dt        -> "for statement must iterate over expression of type 'Range', but it is of type '" ++ show dt ++ "'"
        BreakOutsideLoop        -> "break statement not within loop"
        ContinueOutsideLoop     -> "continue statement not within loop"
        ReturnProcedure g fname -> "cannot return '" ++ show g ++ "' in procedure '" ++ fname ++ "'"
        ReturnType e g fname    -> "expected return type '" ++ show e ++ "' for function '" ++ fname ++ "', but got type '" ++ show g ++ "'"
        -- Operators
        UnaryTypes  op dt      -> "operator '" ++ show op ++ "' does not work with operand (" ++ show dt ++ ")"
        BinaryTypes op (dl,dr) -> "operator '" ++ show op ++ "' does not work with operands (" ++ show dl ++ ", " ++ show dr ++ ")"
        -- General
        WrongCategory iden e g -> "using '" ++ iden ++ "' as if it is a " ++ show e ++ ", but it is a " ++ show g
        NotDefined  iden       -> "identifier '" ++ iden ++ "' has not been defined"
        AlreadyDeclared var p  -> "identifier '" ++ var ++ "' has already been declared at " ++ show p
        StaticError msg        -> msg

--------------------------------------------------------------------------------

data CheckWarning
    = DefinedNotUsed        Identifier
    | DefinedNotImplemented Identifier
    | Warning               String

instance Show CheckWarning where
    show cWarn = case cWarn of
        DefinedNotUsed iden         -> "identifier '" ++ iden ++ "' is defined but never used"
        DefinedNotImplemented fname -> "function '" ++ fname ++ "' is defined but never implemented"
        Warning msg                 -> msg

--------------------------------------------------------------------------------

data CheckState = CheckState
    { table     :: SymTable
    , stack     :: Stack Scope
    , scopeId   :: ScopeNum
    , ast       :: Program
    , loopLvl   :: NestedLevel
    , funcStack :: Stack (DataType, Lexeme Identifier, ScopeNum)
    }

instance Show CheckState where
    show (CheckState t s c a _ _) = showT ++ showS ++ showC ++ showA
        where
            showT = "Symbol Table:\n" ++ show t ++ "\n"
            showS = "Scope Stack:\n"  ++ show s ++ "\n"
            showC = "Scope Number:\t" ++ show c ++ "\n"
            showA = "Program:\n"      ++ show a ++ "\n"

type NestedLevel = Int

--------------------------------------------------------------------------------

errorPos :: CheckError -> Position
errorPos err = case err of
    LError p _ -> p
    PError p _ -> p
    SError p _ -> p
    CWarn  p _ -> p

currentScope :: Checker ScopeNum
currentScope = gets (serial . peek . stack)

----------------------------------------

tellLError :: Position -> LexerError -> Checker ()
tellLError posn err = tell (singleton $ LError posn err)

tellPError :: Position -> ParseError -> Checker ()
tellPError posn err = tell (singleton $ PError posn err)

tellSError :: Position -> StaticError -> Checker ()
tellSError posn err = tell (singleton $ SError posn err)

tellCWarn :: Position -> CheckWarning -> Checker ()
tellCWarn  posn err = tell (singleton $ CWarn  posn err)

----------------------------------------

flags :: CheckReader
flags = empty

initialState :: CheckState
initialState = CheckState
    { table     = emptyTable
    , stack     = initialStack
    , scopeId   = 0
    , ast       = Program DS.empty
    , loopLvl   = 0
    , funcStack = singletonStack (Void, Lex "sapphire" (0,0), -1)
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

getErrors :: CheckWriter -> (Seq CheckError, Seq CheckError, Seq CheckError, Seq CheckError)
getErrors errors = (sorted lexical, sorted parsing, sorted static, sorted warning)
    where
        sorted f = sortBy compare $ filter f errors
        lexical e = case e of
            (LError _ _) -> True
            _            -> False
        parsing e = case e of
            (PError _ _) -> True
            _            -> False
        static e = case e of
            (SError _ _) -> True
            _            -> False
        warning w = case w of
            (CWarn _ _) -> True
            _           -> False

checkWarnings :: Checker ()
checkWarnings = do
    symbols <- gets $ accessible . table
    forM_ symbols $ \(sym, symIs) ->
        forM_ symIs $ \symI -> do
            let dPosn = defPosn symI
            case category symI of
                CatFunction -> do
                    let Just (ValFunction _ body iPosn) = value symI
                        implemented                     = isJust body
                    case (initialized symI, used symI, implemented) of
                        (_    , True , False) -> tellSError dPosn (UsedNotImplemented sym)
                        (False, _    , True ) -> tellSError iPosn (NoReturn sym)
                        (_    , False, True ) -> tellCWarn  dPosn (DefinedNotUsed sym)
                        (_    , False, False) -> tellCWarn  dPosn (DefinedNotImplemented sym)
                        _                     -> return ()
                _ -> unless (used symI) $ tellCWarn  dPosn (DefinedNotUsed sym)

--------------------------------------------------------------------------------

{- |
    Entering a new scope
-}
enterScope :: Checker ()
enterScope = do
    cs <- gets scopeId
    let sc = Scope { serial = cs + 1 }
    modify (\s -> s { stack = push sc (stack s), scopeId = cs + 1 })

{- |
    Exiting a scope that has just been checked
-}
exitScope :: Checker ()
exitScope = modify (\s -> s { stack = snd . pop $ stack s })

----------------------------------------

{- |
    Entering a loop
-}
enterLoop :: Checker ()
enterLoop = modify (\s -> s { loopLvl = loopLvl s + 1 })

{- |
    Exiting a loop
-}
exitLoop :: Checker ()
exitLoop = modify (\s -> s { loopLvl = loopLvl s - 1 })

----------------------------------------

{- |
    Entering a function
-}
enterFunction :: DataType -> Lexeme Identifier -> Checker()
enterFunction dt fnameL =  do
    currSc <- currentScope
    modify (\s -> s { funcStack = push (dt, fnameL, currSc) (funcStack s) })

{- |
    Exiting a function
-}
exitFunction :: Checker ()
exitFunction = modify (\s -> s { funcStack = snd . pop $ funcStack s })

--------------------------------------------------------------------------------

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
        failure = tellSError posn (NotDefined iden) >> return Nothing

{- |
    Modifies a symbol if said symbol exists in the table
-}
modifySymInfo :: Lexeme Identifier -> (SymInfo -> SymInfo) -> Checker ()
modifySymInfo var f = do
    maySc <- getsSymInfo var scopeNum
    maybe (return ()) (modifySymInfoWithScope var f) maySc

{- |
    Modifies a symbol in a specific scope if said symbol exists in the table
-}
modifySymInfoWithScope :: Lexeme Identifier -> (SymInfo -> SymInfo) -> ScopeNum -> Checker ()
modifySymInfoWithScope var@(Lex iden _) f sc = do
    tab <- gets table
    maySi <- getsSymInfo var id
    case maySi of
        Just si -> modify (\s -> s { table = updateWithScope iden sc f tab })
        Nothing -> return ()

{- |
    Changes the value of the variable to the value specified in the symbol table
-}
putValue :: Lexeme Identifier -> Value -> Checker ()
putValue var val = modifySymInfo var (\sym -> sym { value = Just val })

{- |
    Marks the specified variable as initialized
-}
markInitialized :: Lexeme Identifier -> Checker ()
markInitialized var = modifySymInfo var (\sym -> sym { initialized = True })

{- |
    Gets a list of variables, checks which ones need initialization and intialises them.
-}
markAllInitializedUsed :: Seq (Identifier, SymInfo) -> Checker ()
markAllInitializedUsed vars = forM_ vars $ \(var,info) -> do
    let varL = Lex var (defPosn info)
    when (initialized info) $ markInitialized varL
    when (used info)        $ markUsed        varL

{- |
    Marks the specified variable as used
-}
markUsed :: Lexeme Identifier -> Checker ()
markUsed var = modifySymInfo var (\sym -> sym { used = True })

checkMark :: Seq (Seq (Identifier, SymInfo)) -> Checker ()
checkMark = markAllInitializedUsed . checkInitialization

{- |
    Returns the variables in the current scope
-}
getScopeVariables :: Checker (Seq (Identifier, SymInfo))
getScopeVariables = do
    vars <- gets $ accessible . table
    stck <- liftM (fmap serial) $ gets stack
    return $ foldr (func stck) empty vars
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

{- |
    Checks if a variable has been initialized in all possible scopes.
    Maintains used for variables used in at least one scope.
-}
checkInitialization :: Seq (Seq (Identifier, SymInfo)) -> Seq (Identifier, SymInfo)
checkInitialization = foldr1 (zipWith func)
    where
        func :: (Identifier, SymInfo) -> (Identifier, SymInfo) -> (Identifier, SymInfo)
        func (a,sa) (b,sb) = if a /= b
            then error "Checker.checkInitialization: zipping different variables"
            else (a, sa { initialized = initialized sa && initialized sb
                        , used        = used        sa || used        sb
                        })

checkArguments :: Identifier -> Maybe Value -> Seq (Lexeme Expression) -> Position -> Checker ()
checkArguments fname mayVal args posn = maybe failure success mayVal
    where
        failure = error "Checker.checkArguments: function with no SymInfo value"
        success val = do
            let prms = lexInfo <$> parameters val
            dts <- mapM checkExpression args
            unless (length args == length prms && and (zipWith (==) dts prms) || TypeError `elem` dts) $
                tellSError posn (FunctionArguments fname prms dts)

----------------------------------------

{- |
    Adds a declaration to the symbol table.
    Returns a Bool indicating if it was added successfully.
-}
processVariable :: Lexeme Declaration -> Checker Bool
processVariable decl@(Lex (Declaration idenL@(Lex iden _) (Lex t _) c) posn) = do
    (tab,stck) <- gets (table &&& stack)
    sc         <- currentScope
    let info = emptySymInfo {
                dataType = t,
                category = c,
                scopeNum = sc,
                defPosn  = posn
            }
    case lookupWithScope iden stck tab of
        Nothing -> addSymbolCheckingUserDef tab stck info
        Just si
            | scopeNum si == sc -> tellSError posn (AlreadyDeclared iden (defPosn si)) >> return False
            | otherwise         -> addSymbolCheckingUserDef tab stck info
        where
            addSymbolCheckingUserDef :: SymTable -> Stack Scope -> SymInfo -> Checker Bool
            addSymbolCheckingUserDef tab stck info = case t of
                UserDef (Lex udIden _) -> case lookupWithScope udIden stck tab of
                    Nothing -> tellSError posn (UndefinedType udIden) >> return False
                    Just si -> do
                        --let newInfo = info { dataType = dataType si }
                        let fields = getFields $ dataType si
                        addSymbol idenL info
                        let newSymTable = foldl addField emptyTable fields
                        putValue idenL (ValStruct newSymTable)
                        return True
                Array aDtL indexL -> do
                    indexDt <- checkExpression indexL
                    if indexDt == Int
                        then addSymbol idenL info >> return True
                        else do
                            unless (indexDt == TypeError) $ tellSError posn (ArraySizeDataType (lexInfo indexL) indexDt)
                            return False
                _ -> addSymbol idenL info >> return True
            addField :: SymTable -> Field -> SymTable
            addField tab (fIdenL, dtL) =
                let fInfo = emptySymInfo {
                        dataType = lexInfo dtL,
                        category = CatField,
                        scopeNum = 0,
                        defPosn  = posn
                    }
                    in insert (lexInfo fIdenL) fInfo tab

processType :: Lexeme Declaration -> Checker Bool
processType decl@(Lex (Declaration idenL@(Lex iden _) dtL _) posn) = do
    valid <- liftM snd $ foldlM validateFields (DM.empty, True) (getFields $ lexInfo dtL)
    if valid
        then processGeneric decl success
        else return False
    where
        success info si
            | scopeNum si == scopeNum info = tellSError posn (TypeAlreadyDefined iden (defPosn si)) >> return False
            | scopeNum si == -1            = tellSError posn (LanguageTypeRedefine iden) >> return False
            | otherwise                    = addSymbol idenL info >> return True
        validateFields :: (DM.Map Identifier Position, Bool) -> Field -> Checker (DM.Map Identifier Position, Bool)
        validateFields (fieldMap, bool) (Lex fIden fPosn, _) = do
            let (mayPosn, newfMap) = DM.insertLookupWithKey (\_ _ a -> a) fIden fPosn fieldMap
            case mayPosn of
                Just posn -> do
                    tellSError fPosn (AlreadyDeclared fIden posn)
                    return (newfMap, False)
                Nothing   -> return (newfMap, bool)

processFunction :: Lexeme Declaration -> Checker Bool
processFunction decl@(Lex (Declaration idenL@(Lex iden _) _ _) posn) = processGeneric decl success
    where
        success info si
            | scopeNum si == scopeNum info = tellSError posn (FunctionAlreadyDefined iden (defPosn si)) >> return False
            | scopeNum si == -1            = tellSError posn (LanguageFunctionRedefine iden) >> return False
            | otherwise                    = addSymbol idenL info >> return True

processGeneric :: Lexeme Declaration -> (SymInfo -> SymInfo -> Checker Bool) -> Checker Bool
processGeneric (Lex (Declaration idenL@(Lex iden _) (Lex t _) c) posn) success = do
    (tab,stck) <- gets (table &&& stack)
    sc  <- currentScope
    let info = emptySymInfo {
                dataType = t,
                category = c,
                scopeNum = sc,
                defPosn  = posn
            }
    case lookupWithScope iden stck tab of
        Nothing -> addSymbol idenL info >> return True
        Just si -> success info si

--defaultValue :: Lexeme DataType -> Lexeme Identifier -> Checker StBlock
--defaultValue (Lex dt _) iden = case dt of
--    Void       -> return empty
--    Int        -> assign $ LitInt    (Lex 0 posn)
--    Float      -> assign $ LitFloat  (Lex 0.0 posn)
--    Bool       -> assign $ LitBool   (Lex False posn)
--    Char       -> assign $ LitChar   (Lex '\0' posn)
--    String     -> assign $ LitString (Lex [] posn)
--    --Range      -> assign $ LitRange
--    --Type       -> assign $ LitInt (Lex 0 posn)
--    --Union      ->
--    --Record     ->
--    --Array aDT  ->
--    --User udt -> do
--    --    mayRSc <- getsSymInfo udt scopeNum
--    --    maybe failure success mayRSc
--    --        where
--    --            failure = return empty
--    --            success rSc = do
--    --                tab <- gets table
--    --                let fields = toListFilter tab rSc
--    --                -- We have to 'concat' the name and a dot: "iden . fIden" before sending it
--    --                blocks <- mapM (\(fIden, fInfo) -> defaultValue (Lex (dataType fInfo) (defPosn fInfo)) (Lex fIden (defPosn fInfo))) fields
--    --                return $ foldr (><) empty blocks
--    where
--        assign exp = return . singleton $ Lex (StAssign (singleton iden) (Lex exp posn)) posn
--        posn = lexPosn iden

----------------------------------------

{- |
    Checks the validity of each statement of the program, modifying the state.
-}
checkProgram :: Seq CheckError -> Program -> Checker ()
checkProgram lexErrors pr@(Program sts) = do
    modify (\s -> s { ast = pr })
    tell lexErrors
    checkStatements sts
    checkWarnings

checkStatements :: StBlock -> Checker ()
checkStatements = mapM_ checkStatement

getsSymInfoAccess :: forall a . Lexeme Access -> (SymInfo -> a) -> Checker (Maybe a, Identifier, Lexeme Identifier)
getsSymInfoAccess accL@(Lex acc posn) f = do
    let zipper@(topL, ths)   = deepAccess $ focusAccess accL
        VariableAccess idenL = lexInfo topL
    maySi <- getsSymInfo idenL id
    case maySi of
        Just si -> matchType zipper (lexInfo idenL) si >>= return . (\(a,b) -> (a, b, idenL))
        Nothing -> return (Nothing, lexInfo idenL, idenL)
    where
        matchType ::  Zipper -> Identifier -> SymInfo -> Checker (Maybe a, Identifier)
        matchType zipper str si = case zipper of
                (_, [] ) -> return (Just (f si), str)
                (_, ths) -> case lexInfo (head ths) of
                    HistoryArray  indexL -> case dataType si of
                            Array inDtL _ -> do
                                indexDt <- checkExpression indexL
                                if indexDt == Int
                                    then do
                                        let newSi  = si { dataType = lexInfo inDtL }
                                            newStr = str ++ "[" ++ showIndex (lexInfo indexL) ++ "]"
                                        matchType (fromJust $ backAccess zipper) newStr newSi
                                    else do
                                        unless (indexDt == TypeError) $ tellSError posn (IndexDataType (lexInfo indexL) indexDt)
                                        return (Nothing, str)
                            dt            -> tellSError posn (VariableNonArray str dt) >> return (Nothing, str)
                    HistoryStruct fieldNameL -> case dataType si of
                        UserDef dtIdenL -> do
                            uDefDt <- liftM fromJust $ getsSymInfo dtIdenL dataType
                            let fields = getFields uDefDt
                            case find ((lexInfo fieldNameL==) . lexInfo . fst) fields of
                                Just fieldL -> do
                                    let newSi  = si { dataType = lexInfo (snd fieldL) }
                                        newStr = str ++ "." ++ lexInfo fieldNameL
                                    matchType (fromJust $ backAccess zipper) newStr newSi
                                Nothing -> tellSError posn (StructNoField (lexInfo dtIdenL) (lexInfo fieldNameL)) >> return (Nothing, str)
                        dt -> tellSError posn (VariableNonStruct str dt) >> return (Nothing, str)

{- |
    Checks the validity of a statement, modifying the state.
-}
checkStatement :: Lexeme Statement -> Checker () -- Maybe it should return something else?
checkStatement (Lex st posn) = case st of
    StNoop -> return ()

    StAssign accL exprL -> do
        (mayDt, accStr, topL) <- getsSymInfoAccess accL dataType
        exprDt        <- checkExpression exprL
        case mayDt of
            Just idenDt -> do
                markInitialized topL
                when (idenDt /= exprDt && exprDt /= TypeError) $
                    tellSError posn (InvalidAssignType accStr idenDt exprDt)
            -- The error has already been reported
            Nothing -> return ()

    StDeclaration dcl -> void $ processVariable dcl

    StStructDefinition dtL@(Lex dt posn) -> do
        let idenL = case dt  of
                Record idenL _ -> idenL
                Union  idenL _ -> idenL
        defined <- processType $ Lex (Declaration idenL dtL CatUserDef) posn
        when defined $ markInitialized idenL

    StReturn ex -> do
        dt <- checkExpression ex
        (st, fnameL@(Lex fname _), sc) <- liftM peek $ gets funcStack
        if st == Void
            then tellSError posn (ReturnProcedure dt fname)
            else do
                when (st /= dt && dt /= TypeError) $ tellSError posn (ReturnType st dt fname)
                -- Marks the function initialized, in the correct scope
                modifySymInfoWithScope fnameL (\sym -> sym { initialized = True }) sc

    StFunctionDef decl@(Lex (Declaration iden _ _) _) dts -> do
        defined <- processFunction decl
        when defined $ putValue iden $ ValFunction dts Nothing (0,0)

    StFunctionImp fnameL@(Lex fname _) params body -> do
        currSc <- currentScope
        maySi <- getsSymInfo fnameL id
        case maySi of
            Just si -> do
                let val    = value si
                    sameSc = currSc == scopeNum si
                    catSi  = category si
                    initSi = initialized si
                    dPosn  = defPosn si
                    dt     = dataType si
                case (val, sameSc, catSi, initSi) of
                    (Just v, True, CatFunction, False) -> do
                        putValue fnameL $ v { impl = Just body, implPosn = posn }

                        before <- getScopeVariables
                        enterFunction dt fnameL
                        enterScope
                        -- For procedures
                        when (dt == Void) $ markInitialized fnameL

                        forM_ (zip params (parameters v)) $ \(varL,dt) -> do
                            processVariable $ Declaration varL dt CatParameter <$ varL
                            markInitialized varL

                        checkStatements body
                        exitScope
                        exitFunction
                        varBody <- getScopeVariables

                        putScopeVariables before
                        -- Only marks 'used'
                        checkMark $ fromList [varBody, before]
                        let functions   = filter func varBody
                            func (_,si) = category si == CatFunction && scopeNum si == currSc
                        forM_ functions $ \(iden,si) ->
                            when (initialized si) $ markInitialized $ Lex iden (defPosn si)

                    -- If value is Nothing it means that it is not a function
                    (Nothing,_,cat,_) -> tellSError posn (WrongCategory fname CatFunction cat)
                    (Just v,_,_,True) -> tellSError posn (AlreadyImplemented fname (implPosn v))
                    -- Must implement functions in the same scope in which they are defined
                    (_,False,_,_)     -> tellSError posn (ImpInDefScope fname dPosn)
            Nothing -> tellSError posn (FunctionNotDefined fname)

    StFunctionCall fnameL@(Lex fname _) args -> do
        maySi <- getsSymInfo fnameL (\si -> (category si, value si, dataType si))
        case maySi of
            Just si -> do
                markUsed fnameL
                case si of
                    (CatFunction,mayVal,Void) -> checkArguments fname mayVal args posn
                    (CatFunction,_,_)         -> tellSError posn (FunctionAsStatement fname)
                    (ct,_,_)                  -> tellSError posn (WrongCategory fname CatFunction ct)
            Nothing -> return ()

    StRead vars -> forM_ vars $ \accL -> do
        (maySi, _, topL) <- getsSymInfoAccess accL id
        case maySi of
            Just _  -> markInitialized topL
            Nothing -> return ()

    StPrint exs -> mapM_ checkExpression exs

    StIf cnd success failure -> do
        dt <- checkExpression cnd
        unless (dt == Bool || dt == TypeError) $ tellSError posn (ConditionDataType dt)
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

        checkMark $ fromList [varSucc,varFail]

    StCase ex cs othrw -> do
        dt     <- checkExpression ex
        before <- getScopeVariables

        varScopes <- forM cs $ \(Lex (When wexps sts) wposn) -> do
            forM_ wexps $ checkExpression >=> \wd ->        -- forM_ wexps $ \wexp -> checkExpression wexp >>= \wd ->
                unless (wd == dt || dt == TypeError || wd == TypeError) $
                    tellSError wposn (CaseWhenDataType dt wd)

            enterScope
            checkStatements sts
            exitScope
            varWhen <- getScopeVariables
            putScopeVariables before

            return varWhen

        enterScope
        checkStatements othrw
        exitScope
        varOtherwise <- getScopeVariables

        putScopeVariables before
        checkMark $ varScopes |> varOtherwise

    StLoop rep cnd body -> do
        enterScope >> enterLoop
        checkStatements rep
        exitLoop   >> exitScope

        checkExpression cnd >>= \dt ->
            unless (dt == Bool || dt == TypeError) $ tellSError posn (ConditionDataType dt)

        before <- getScopeVariables

        enterScope >> enterLoop
        checkStatements body
        exitLoop   >> exitScope
        varBody <- getScopeVariables

        putScopeVariables before
        -- Only marks 'used'
        checkMark $ fromList [varBody, before]

    StFor var rng body -> do
        dt <- checkExpression rng
        unless (dt == Range || dt == TypeError) $ tellSError posn (ForInDataType dt)

        before <- getScopeVariables

        enterScope >> enterLoop
        processVariable $ Declaration var (Int <$ var) CatVariable <$ var
        markInitialized var
        checkStatements body
        exitLoop >> exitScope
        varBody <- getScopeVariables

        putScopeVariables before
        -- Only marks 'used'
        checkMark $ fromList [varBody, before]

    StBreak -> do
        loopL <- gets loopLvl
        unless (loopL > 0) $ tellSError posn BreakOutsideLoop

    StContinue -> do
        loopL <- gets loopLvl
        unless (loopL > 0) $ tellSError posn ContinueOutsideLoop

{- |
    Checks the validity of an expression and returns it's value.
-}
checkExpression :: Lexeme Expression -> Checker DataType
checkExpression (Lex e posn) = case e of
    Variable accL -> do
        (maySi, _, topL) <- getsSymInfoAccess accL id
        case maySi of
            Just si -> do
                markUsed topL
                let ini = initialized si
                    dt  = dataType si
                case category si of
                    CatVariable -> do
                        unless ini $ tellSError posn (VariableNotInitialized $ lexInfo topL)
                        return dt
                    CatParameter -> do
                        unless ini $ tellSError posn (VariableNotInitialized $ lexInfo topL)
                        return dt
                    ct -> do
                        tellSError posn (WrongCategory (lexInfo topL) CatVariable ct)
                        return TypeError
            Nothing -> return TypeError

    FunctionCall fnameL@(Lex fname _) args -> do
        maySi <- getsSymInfo fnameL id
        case maySi of
            Just si -> do
                markUsed fnameL
                case (category si, dataType si) of
                    (CatFunction, Void) -> do
                        tellSError posn (ProcedureInExpression fname)
                        return TypeError
                    (CatFunction, dt) -> do
                        checkArguments fname (value si) args posn
                        return dt
                    (ct,_) -> do
                        tellSError posn (WrongCategory fname CatFunction ct)
                        return TypeError
            Nothing -> return TypeError

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
                -- If there's a 'Void' an error has already been raised about this.
                unless (TypeError == ldt || TypeError == rdt) $
                    tellSError posn (BinaryTypes op (ldt,rdt))
                return TypeError
            else return . snd $ index dts 0

    ExpUnary (Lex op _) expr  -> do
        dt   <- checkExpression expr
        let dts = filter ((dt==) . fst) $ unaryOperation op
        if null dts
            then do
                tellSError posn (UnaryTypes op dt)
                return TypeError
            else return . snd $ index dts 0
