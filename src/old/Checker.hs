{-# LANGUAGE ScopedTypeVariables #-}
module Checker where

import           Language
import           SymbolTable

import           Control.Arrow          ((&&&))
import           Control.Monad.Identity (Identity (..), runIdentity)
import           Control.Monad.RWS      hiding (forM, forM_, mapM, mapM_)
import           Data.Foldable          as DF (all, and, concatMap, elem, find,
                                               foldl, foldlM, foldr, foldr1,
                                               forM_, mapM_, notElem, sum,
                                               toList)
import           Data.Function          (on)
import           Data.Functor           ((<$), (<$>))
import           Data.List              (intercalate)
import qualified Data.Map               as DM (Map, empty, fromList,
                                               insertLookupWithKey, lookup)
import           Data.Maybe             (fromJust, isJust, isNothing)
import           Data.Sequence          as DS (Seq, ViewL ((:<), EmptyL), empty,
                                               filter, fromList, index, length,
                                               null, singleton, sortBy, viewl,
                                               zip, zipWith, (<|), (><), (|>))
import           Data.Traversable       as DT (forM, mapM)
import           GHC.Generics
import           Prelude                as P hiding (all, and, concatMap, elem,
                                              filter, foldl, foldr, foldr1,
                                              length, lookup, mapM, mapM_,
                                              notElem, null, sum, zip, zipWith)

--------------------------------------------------------------------------------

type Checker a = RWST CheckReader CheckWriter CheckState Identity a

--------------------------------------------------------------------------------

data CheckReader = CheckReader
    { flags :: Seq Flag
    , arch  :: Architecture
    }

data Flag = OutputFile String | SupressWarnings | AllWarnings
    deriving (Eq)

data Architecture = Arch
    { archName :: String
    , widths   :: DM.Map DataType Width
    } deriving (Show)

instance Eq Architecture where
    Arch n _ == Arch n2 _ = n == n2

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
    = UnexpectedToken String
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
    | ImpureArraySize        Expression
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
        --ImpureArraySize        expr      -> "array size expression '" ++ showIndex expr ++ "' is 'impure'"
        ImpureArraySize        expr      -> "array size expression '" ++ showIndex expr ++ "' must be an 'Int' literal"
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
    , offsStack :: Stack Offset
    }

instance Show CheckState where
    show (CheckState t s c a _ _ _) = showT ++ showS ++ showC ++ showA
        where
            showT = show t ++ "\n"
            showS = "Scope Stack:\n"  ++ show s ++ "\n"
            showC = "Scope Number:\t" ++ show c ++ "\n"
            showA = show a ++ "\n"

type NestedLevel = Int

--------------------------------------------------------------------------------

errorPos :: CheckError -> Position
errorPos err = case err of
    LError p _ -> p
    PError p _ -> p
    SError p _ -> p
    CWarn  p _ -> p

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

initialReader :: CheckReader
initialReader = CheckReader
    { flags  = empty
    , arch   = defaultArchitecture
    }

defaultArchitecture :: Architecture
defaultArchitecture = Arch
    { archName = "mips"
    , widths = DM.fromList
        [ (Int       , 32)
        , (Float     , 32)
        {-, (Double    , 64)-}
        , (Char      , 8)
        , (Bool      , 8)
        {-, ("pointer" , 32)-}
        ]
    }

getCurrentArchitecture :: Checker Architecture
getCurrentArchitecture = asks arch

getPrimitiveDataTypeWidth :: DataType -> Checker Width
getPrimitiveDataTypeWidth dt = asks (fromJust . DM.lookup dt . widths . arch)

getDataTypeWidth :: DataType -> Checker Width
getDataTypeWidth dt = case dt of
    Void              -> return 0
    TypeError         -> error "Checker.getDataTypeWidth: asking for width of TypeError"
    String w          -> return w
    Array dtL sizeL w -> if w /= 0
        then return w
        else do
            (sizeDt, sizePr) <- checkExpression sizeL
            dtW              <- getDataTypeWidth (lexInfo dtL)

            unless sizePr $ tellSError (lexPosn sizeL) (ImpureArraySize (lexInfo sizeL))

            if sizeDt == Int
                then case lexInfo sizeL of
                    LitInt intL -> return $ dtW * (lexInfo intL)
                    _           -> tellSError (lexPosn sizeL) (ImpureArraySize $ lexInfo sizeL) >> return 0
                else do
                    unless (sizeDt == TypeError) $ tellSError (lexPosn sizeL) (ArraySizeDataType (lexInfo sizeL) sizeDt)
                    return 0
    Record _ _ w -> return w
    Union _ _ w  -> return w
    UserDef udIdenL -> do
        mayDt <- getsSymInfoWithoutError udIdenL dataType
        case mayDt of
            Just udDt -> getDataTypeWidth udDt
            Nothing   -> error "Checker.getDataTypeWidth: lookup of existing struct returned Nothing"
    _ -> getPrimitiveDataTypeWidth dt

----------------------------------------

initialState :: CheckState
initialState = CheckState
    { table     = initialTable
    , stack     = initialStack
    , scopeId   = 0
    , ast       = Program DS.empty
    , loopLvl   = 0
    , funcStack = singletonStack (Void, Lex "sapphire" (0,0), -1)
    , offsStack = singletonStack 0
    }

----------------------------------------

runProgramChecker :: Checker a -> (CheckState, CheckWriter)
runProgramChecker = (\(_,s,w) -> (s,w)) . runChecker

runChecker :: Checker a -> (a, CheckState, CheckWriter)
runChecker = runIdentity . flip (`runRWST` initialReader) initialState

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
                    let Just (ValFunction _ body iPosn _) = value symI
                        implemented                     = isJust body
                    case (initial symI, used symI, implemented) of
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
    addOffsetBase

{- |
    Exiting a scope that has just been checked
-}
exitScope :: Checker Offset
exitScope = modify (\s -> s { stack = snd . pop $ stack s }) >> removeOffsetBase

{- |
    Gets the current scope on top of the stack
-}
currentScope :: Checker ScopeNum
currentScope = gets (serial . peek . stack)

----------------------------------------

addOffsetBase :: Checker ()
addOffsetBase = modify (\s -> s { offsStack = push 0 (offsStack s) })

removeOffsetBase :: Checker Offset
removeOffsetBase = do
    (offset, stck) <- liftM pop $ gets offsStack
    modify (\s -> s { offsStack = stck })
    return offset

currentOffset :: Checker Offset
currentOffset = gets (peek . offsStack)

modifyOffset :: (Width -> Offset) -> Checker ()
modifyOffset f = do
    co <- removeOffsetBase
    modify (\s -> s { offsStack = push (f co) (offsStack s) })


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

{- |
    Gets the current function on top of the stack
-}
currentFunction :: Checker (DataType, Lexeme Identifier, ScopeNum)
currentFunction = liftM peek $ gets funcStack

{- |
    Sets the current function as impure
-}
impureFunction :: Checker ()
impureFunction = do
    (_,fnameL, sc) <- currentFunction
    modifySymInfoWithScope fnameL (\sym -> sym { pure = False }) sc

--------------------------------------------------------------------------------

{- |
    Adds a symbol to the Checker's symbol table
-}
addSymbol :: Lexeme Identifier -> SymInfo -> Checker ()
addSymbol (Lex var _) info = modify $ \s -> s { table = insert var info (table s)}

{- |
    Gets a symbol's attribute if said symbol exists in the table,
    otherwise reports an error and returns Nothing
-}
getsSymInfo :: Lexeme Identifier -> (SymInfo -> a) -> Checker (Maybe a)
getsSymInfo idenL@(Lex iden posn) f = do
    maySi <- getsSymInfoWithoutError idenL f
    maybe failure (return . Just) maySi
    where
        failure = tellSError posn (NotDefined iden) >> return Nothing

{- |
    Gets a symbol's attribute if said symbol exists in the table, otherwise Nothing
-}
getsSymInfoWithoutError :: Lexeme Identifier -> (SymInfo -> a) -> Checker (Maybe a)
getsSymInfoWithoutError (Lex iden _) f = do
    (tab, stck) <- gets (table &&& stack)
    return $ f <$> lookupWithScope iden stck tab -- f <$> == maybe Nothing (Just . f)

{- |
    Gets an Access' attribute if said symbol exists in the table, otherwise Nothing.
-}
getsSymInfoAccess :: forall a . Lexeme Access -> (SymInfo -> a) -> Checker (Maybe a, Identifier, Lexeme Identifier)
getsSymInfoAccess accL@(Lex acc posn) f = do
    let zipper@(topL, ths)   = deepAccess $ focusAccess accL
        VariableAccess idenL = lexInfo topL
    maySi <- getsSymInfo idenL id
    case maySi of
        Just si -> liftM (\(a,b) -> (a, b, idenL)) $ matchType zipper (lexInfo idenL) si
        Nothing -> return (Nothing, lexInfo idenL, idenL)
    where
        matchType ::  Zipper -> Identifier -> SymInfo -> Checker (Maybe a, Identifier)
        matchType zipper str si = case zipper of
                (_, [] ) -> return (Just (f si), str)
                (_, ths) -> case lexInfo (head ths) of
                    HistoryArray  indexL -> case dataType si of
                            Array inDtL _ _ -> do
                                (indexDt, _) <- checkExpression indexL
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
                        UserDef udIdenL -> do
                            udDt <- liftM fromJust $ getsSymInfoWithoutError udIdenL dataType
                            case find ((lexInfo fieldNameL ==) . lexInfo . fst) (getFields udDt) of
                                Just fieldL -> do
                                    let newSi  = si { dataType = lexInfo (snd fieldL) }
                                        newStr = str ++ "." ++ lexInfo fieldNameL
                                    matchType (fromJust $ backAccess zipper) newStr newSi
                                Nothing -> tellSError posn (StructNoField (lexInfo udIdenL) (lexInfo fieldNameL)) >> return (Nothing, str)
                        dt -> tellSError posn (VariableNonStruct str dt) >> return (Nothing, str)


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
markInitialized var = modifySymInfo var (\sym -> sym { initial = True })

----------------------------------------

{- |
    Marks the specified variable as impure
-}
markImpure :: Lexeme Identifier -> Checker ()
markImpure var = modifySymInfo var (\sym -> sym { pure = False })

{- |
    Marks the specified variable as pure
-}
markPure :: Lexeme Identifier -> Checker ()
markPure var = modifySymInfo var (\sym -> sym { pure = True })

{- |
    Marks the specified variable as the specified pureness
-}
markPureness :: Lexeme Identifier -> Bool -> Checker ()
markPureness var p = modifySymInfo var (\sym -> sym { pure = p })

----------------------------------------

{- |
    Gets a list of variables, checks which ones need initialization and intialises them.
-}
markAllInitializedUsed :: Seq (Identifier, SymInfo) -> Checker ()
markAllInitializedUsed vars = forM_ vars $ \(var,info) -> do
    let varL = Lex var (defPosn info)
    when (initial info) $ markInitialized varL
    when (used info)        $ markUsed        varL

{- |
    Marks the specified variable as used
-}
markUsed :: Lexeme Identifier -> Checker ()
markUsed var = modifySymInfo var (\sym -> sym { used = True })

checkMark :: Seq (Seq (Identifier, SymInfo)) -> Checker ()
checkMark = markAllInitializedUsed . checkInitializationUsed

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
checkInitializationUsed :: Seq (Seq (Identifier, SymInfo)) -> Seq (Identifier, SymInfo)
checkInitializationUsed = foldr1 (zipWith func)
    where
        func :: (Identifier, SymInfo) -> (Identifier, SymInfo) -> (Identifier, SymInfo)
        func (a,sa) (b,sb) = if a /= b
            then error "Checker.checkInitializationUsed: zipping different variables"
            else (a, sa { initial = initial sa && initial sb
                        , used    = used    sa || used        sb
                        , pure    = pure    sa && pure        sb
                        })

checkArguments :: Lexeme Identifier -> Maybe Value -> Seq (Lexeme Expression) -> Position -> Checker ()
checkArguments fnameL mayVal args posn = maybe failure success mayVal
    where
        failure = error "Checker.checkArguments: function with no SymInfo value"
        success val = do
            let prms = lexInfo <$> parameters val
            dts <- mapM (liftM fst . checkExpression) args
            unless (length args == length prms && and (zipWith (==) dts prms) || TypeError `elem` dts) $
                tellSError posn (FunctionArguments (lexInfo fnameL) prms dts)

----------------------------------------

{- |
    Adds a declaration to the symbol table.
    Returns a Bool indicating if it was added successfully.
-}
processVariable :: Lexeme Declaration -> Checker Bool
processVariable decl@(Lex (Declaration idenL (Lex dt _) c) posn) = do
    sc  <- currentScope
    off <- currentOffset
    let info = emptySymInfo
            { dataType = dt
            , category = c
            , scopeNum = sc
            , defPosn  = posn
            , offset   = off
            }
    maySi <- getsSymInfoWithoutError idenL id
    case maySi of
        Nothing -> do
            (valid, width) <- addSymbolCheckingAccess info
            when valid $ do
                modifyOffset (+ width)
                when (isAccess $ dataType info) $ markInitialized idenL
            return valid
        Just si
            | scopeNum si == sc -> tellSError posn (AlreadyDeclared (lexInfo idenL) (defPosn si)) >> return False
            | otherwise         -> do
                (valid, width) <- addSymbolCheckingAccess info
                when valid $ modifyOffset (+ width)
                return valid
    where
        isAccess :: DataType -> Bool
        isAccess dt = case dt of
            UserDef _   -> True
            Array _ _ _ -> True
            _           -> False

        addSymbolCheckingAccess :: SymInfo -> Checker (Bool, Width)
        addSymbolCheckingAccess info = do
            (valid, newDt, width) <- checkAccess $ dataType info
            when valid $ addSymbol idenL (info { dataType = newDt })
            return (valid, width)
        checkAccess :: DataType -> Checker (Bool, DataType, Width)
        checkAccess dt = case dt of
            UserDef udIdenL -> do
                mayUdDt <- getsSymInfoWithoutError udIdenL dataType
                case mayUdDt of
                    Nothing -> tellSError posn (UndefinedType (lexInfo udIdenL)) >> return (False, dt, 0)
                    Just udDt -> markUsed udIdenL >> liftM ((,,) True dt) (getDataTypeWidth udDt)
            Array aDtL sizeL _ -> do
                (indexDt, indexPr) <- checkExpression sizeL
                (indexBool, size) <- if indexDt == Int
                    then do
                        -- This will not happen until 'pureness' is well implemented, so we just check if it is a literal
                        unless indexPr $ tellSError (lexPosn sizeL) (ImpureArraySize (lexInfo sizeL))
                        -- Checks if the expression is a literal, this is temporal
                        case lexInfo sizeL of
                            LitInt sizeL -> return (True, lexInfo sizeL)
                            _            -> tellSError (lexPosn sizeL) (ImpureArraySize (lexInfo sizeL)) >> return (False, 0)
                    else do
                        unless (indexDt == TypeError) $ tellSError (lexPosn sizeL) (ArraySizeDataType (lexInfo sizeL) indexDt)
                        return (False, 0)
                (dtBool, dtDt, dtWidth) <- checkAccess $ lexInfo aDtL
                let newWidth = dtWidth * size
                return (dtBool && indexBool, Array (dtDt <$ aDtL) sizeL newWidth, newWidth)
            _ -> do
                dtW <- getDataTypeWidth dt
                return (True, dt, dtW)

processType :: Lexeme Declaration -> Checker Bool
processType declL@(Lex (Declaration idenL@(Lex iden _) (Lex dt _) c) posn) = do
    (valid, width, _) <- foldlM validateFields (True, 0, DM.empty) (getFields dt)
    if valid
        then do
            sc  <- currentScope
            let info = emptySymInfo {
                        dataType = setWidth width dt,
                        category = c,
                        scopeNum = sc,
                        defPosn  = posn
                    }
            maySi <- getsSymInfoWithoutError idenL id
            case maySi of
                Nothing -> addSymbol idenL info >> return True
                Just si -> success info si
        else return False
    where
        success info si
            | scopeNum si == scopeNum info = tellSError posn (TypeAlreadyDefined iden (defPosn si)) >> return False
            | scopeNum si == -1            = tellSError posn (LanguageTypeRedefine iden) >> return False
            | otherwise                    = addSymbol idenL info >> return True
        setWidth :: Width -> DataType -> DataType
        setWidth width dt = case dt of
            Record idenL fields _ -> Record idenL fields width
            Union idenL fields _  -> Union  idenL fields width
            _                     -> error "Checker.processType.setWidth: setting width to non user-defined DataType"
        validateFields :: (Bool, Width, DM.Map Identifier Position) -> Field -> Checker (Bool, Width, DM.Map Identifier Position)
        validateFields (valid, width, fieldMap) (Lex fIden fPosn, dtL) = do
            let (mayPosn, newfMap) = DM.insertLookupWithKey (\_ _ a -> a) fIden fPosn fieldMap
            case mayPosn of
                Just posn -> do
                    tellSError fPosn (AlreadyDeclared fIden posn)
                    return (False, 0, newfMap)
                Nothing   -> case lexInfo dtL of
                    UserDef udIdenL -> do
                        mayDt <- getsSymInfoWithoutError udIdenL dataType
                        case mayDt of
                            Just udDt -> do
                                udDtW <- getDataTypeWidth udDt
                                return (valid, width + udDtW, newfMap)
                            Nothing -> do
                                tellSError fPosn (UndefinedType (lexInfo udIdenL))
                                return (False, 0, newfMap)
                    Array aDtL sizeL _ -> case lexInfo sizeL of
                        LitInt sizeL -> do
                            dtWidth <- getDataTypeWidth (lexInfo aDtL)
                            return (True, dtWidth * lexInfo sizeL, newfMap)
                        _           -> tellSError (lexPosn sizeL) (ImpureArraySize (lexInfo sizeL)) >> return (False, 0, newfMap)
                    _ -> do
                        dtW <- getDataTypeWidth (lexInfo dtL)
                        return (valid, width + dtW, newfMap)

processFunction :: Lexeme Declaration -> Checker Bool
processFunction decl@(Lex (Declaration idenL@(Lex iden _) _ _) posn) = processGeneric decl success
    where
        success info si
            | scopeNum si == scopeNum info = tellSError posn (FunctionAlreadyDefined iden (defPosn si)) >> return False
            | scopeNum si == -1            = tellSError posn (LanguageFunctionRedefine iden) >> return False
            | otherwise                    = addSymbol idenL info >> return True

processGeneric :: Lexeme Declaration -> (SymInfo -> SymInfo -> Checker Bool) -> Checker Bool
processGeneric (Lex (Declaration idenL (Lex t _) c) posn) success = do
    sc  <- currentScope
    let info = emptySymInfo {
                dataType = t,
                category = c,
                scopeNum = sc,
                defPosn  = posn
            }
    maySi <- getsSymInfoWithoutError idenL id
    case maySi of
        Nothing -> addSymbol idenL info >> return True
        Just si -> success info si

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

{- |
    Checks the validity of a statement, modifying the state.
-}
checkStatement :: Lexeme Statement -> Checker () -- Maybe it should return something else?
checkStatement (Lex st posn) = case st of
    StNoop -> return ()

    StAssign accL exprL -> do
        (mayDtPr, accStr, topL) <- getsSymInfoAccess accL (dataType &&& pure)
        (exprDt, _)        <- checkExpression exprL
        case mayDtPr of
            Just (accDt, accPr) -> do
                markInitialized topL
                when (accDt /= exprDt && exprDt /= TypeError) $
                    tellSError posn (InvalidAssignType accStr accDt exprDt)
            -- The error has already been reported
            Nothing -> return ()

    StDeclaration dcl -> void $ processVariable dcl

    StStructDefinition dtL@(Lex dt posn) -> do
        let idenL = case dt of
                Record idenL _ _ -> idenL
                Union  idenL _ _ -> idenL
        defined <- processType $ Lex (Declaration idenL dtL CatUserDef) posn
        when defined $ markInitialized idenL

    StReturn ex -> do
        (dt, _) <- checkExpression ex
        (st, fnameL@(Lex fname _), sc) <- currentFunction
        if st == Void
            then tellSError posn (ReturnProcedure dt fname)
            else do
                when (st /= dt && dt /= TypeError) $ tellSError posn (ReturnType st dt fname)
                -- Marks the function initialized, in the correct scope
                modifySymInfoWithScope fnameL (\sym -> sym { initial = True }) sc

    StFunctionDef decl@(Lex (Declaration idenL _ _) _) dts -> do
        defined <- processFunction decl
        when defined $ putValue idenL $ ValFunction dts Nothing (0,0) 0

    StFunctionImp fnameL@(Lex fname _) params body -> do
        currSc <- currentScope
        maySi <- getsSymInfoWithoutError fnameL id
        case maySi of
            Just si -> do
                let sameSc = currSc == scopeNum si
                    dPosn  = defPosn si
                    dt     = dataType si
                case (value si, sameSc, category si) of
                    (Just val, True, CatFunction) -> case val of
                        -- Has already been implemented
                        ValFunction _ (Just _) iPosn _ -> tellSError posn (AlreadyImplemented fname iPosn)
                        ValFunction vParams Nothing _ _ -> do
                            before <- getScopeVariables
                            enterFunction dt fnameL >> enterScope   -- Order is important here (enterFunction uses the currentScope)
                            -- For procedures
                            when (dt == Void) $ markInitialized fnameL

                            forM_ (zip params vParams) $ \(varL, dt) -> do
                                processVariable $ Declaration varL dt CatParameter <$ varL
                                markInitialized varL

                            checkStatements body

                            width <- exitScope
                            exitFunction
                            varBody <- getScopeVariables

                            putScopeVariables before
                            -- Only marks 'used'
                            checkMark $ fromList [varBody, before]

                            -- Marks the function as 'initialized' if it returns its value
                            let functions    = filter func varBody
                                func (_, si) = category si == CatFunction && scopeNum si == currSc
                            forM_ functions $ \(iden,si) ->
                                when (initial si) $ markInitialized $ Lex iden (defPosn si)

                            -- Setting body, posn and width
                            putValue fnameL $ val { impl = Just body, implPosn = posn, implWidth = width }
                    -- Must implement functions in the same scope in which they are defined
                    (_, False, CatFunction) -> tellSError posn (ImpInDefScope fname dPosn)
                    -- It is not a function
                    (_, _    , cat        ) -> tellSError posn (WrongCategory fname CatFunction cat)
            Nothing -> tellSError posn (FunctionNotDefined fname)

    StProcedureCall fnameL args -> do
        maySi <- getsSymInfo fnameL (\si -> (category si, value si, dataType si))
        case maySi of
            Just si -> do
                markUsed fnameL
                case si of
                    (CatFunction,mayVal,Void) -> checkArguments fnameL mayVal args posn
                    (CatFunction,_     ,_   ) -> tellSError posn (FunctionAsStatement (lexInfo fnameL))
                    (cat        ,_     ,_   ) -> tellSError posn (WrongCategory (lexInfo fnameL) CatFunction cat)
            Nothing -> return ()

    StRead vars -> forM_ vars $ \accL -> do
        (maySi, _, topL) <- getsSymInfoAccess accL id
        case maySi of
            Just _  -> markInitialized topL
            Nothing -> return ()

    StPrint exs -> mapM_ checkExpression exs

    StIf cnd success failure -> do
        (dt, _) <- checkExpression cnd
        unless (dt == Bool || dt == TypeError) $ tellSError posn (ConditionDataType dt)
        before <- getScopeVariables

        enterScope
        checkStatements success
        exitScope >>= \width -> modifyOffset (+ width)
        varSucc <- getScopeVariables
        putScopeVariables before

        enterScope
        checkStatements failure
        exitScope >>= \width -> modifyOffset (+ width)
        varFail <- getScopeVariables
        putScopeVariables before

        checkMark $ fromList [varSucc,varFail]

    StCase ex cs othrw -> do
        (dt, _) <- checkExpression ex
        before  <- getScopeVariables

        varScopes <- forM cs $ \(Lex (When wexps sts) wposn) -> do
            forM_ wexps $ checkExpression >=> \(wd, _) ->        -- forM_ wexps $ \wexp -> checkExpression wexp >>= \wd ->
                unless (wd == dt || dt == TypeError || wd == TypeError) $
                    tellSError wposn (CaseWhenDataType dt wd)

            enterScope
            checkStatements sts
            exitScope >>= \width -> modifyOffset (+ width)
            varWhen <- getScopeVariables
            putScopeVariables before

            return varWhen

        enterScope
        checkStatements othrw
        exitScope >>= \width -> modifyOffset (+ width)
        varOtherwise <- getScopeVariables

        putScopeVariables before
        checkMark $ varScopes |> varOtherwise

    StLoop rep cnd body -> do
        enterScope >> enterLoop
        checkStatements rep
        exitLoop   >> exitScope >>= \width -> modifyOffset (+ width)

        checkExpression cnd >>= \(dt, _) ->
            unless (dt == Bool || dt == TypeError) $ tellSError posn (ConditionDataType dt)

        before <- getScopeVariables

        enterScope >> enterLoop
        checkStatements body
        exitLoop   >> exitScope >>= \width -> modifyOffset (+ width)
        varBody <- getScopeVariables

        putScopeVariables before
        -- Only marks 'used'
        checkMark $ fromList [varBody, before]

    StFor var rng body -> do
        (dt, _) <- checkExpression rng
        unless (dt == Range || dt == TypeError) $ tellSError posn (ForInDataType dt)

        before <- getScopeVariables

        enterScope >> enterLoop
        processVariable $ Declaration var (Int <$ var) CatVariable <$ var
        markInitialized var
        checkStatements body
        exitLoop >> exitScope >>= \width -> modifyOffset (+ width)
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
checkExpression :: Lexeme Expression -> Checker (DataType, Pure)
checkExpression (Lex e posn) = case e of
    Variable accL -> do
        (maySi, _, topL) <- getsSymInfoAccess accL id
        case maySi of
            Just si -> do
                markUsed topL
                let cat = category si
                if cat == CatVariable || cat == CatParameter
                    then do
                        unless (initial si) $ tellSError posn (VariableNotInitialized $ lexInfo topL)
                        return (dataType si, pure si)
                    else do
                        tellSError posn (WrongCategory (lexInfo topL) CatVariable cat)
                        return (TypeError, pure si)
            Nothing -> return (TypeError, True)

    FunctionCall fnameL args -> do
        maySi <- getsSymInfo fnameL id
        case maySi of
            Just si -> do
                markUsed fnameL
                case (category si, dataType si) of
                    (CatFunction, Void) -> do
                        tellSError posn (ProcedureInExpression (lexInfo fnameL))
                        return (TypeError, pure si)
                    (CatFunction, dt  ) -> do
                        checkArguments fnameL (value si) args posn
                        return (dt, pure si)
                    (cat        , _   ) -> do
                        tellSError posn (WrongCategory (lexInfo fnameL) CatFunction cat)
                        return (TypeError, pure si)
            Nothing -> return (TypeError, True)

    LitInt _      -> return (Int, True)

    LitFloat _    -> return (Float, True)

    LitBool _     -> return (Bool, True)

    LitChar _     -> return (Char, True)

    LitString _ w -> return (String w, True)

    ExpBinary (Lex op _) l r  -> do
        lInfo@(lDt, lPr) <- checkExpression l
        rInfo@(rDt, rPr) <- checkExpression r
        maybe (failure lInfo rInfo) (success lInfo rInfo) $ find (((lDt,rDt) ==) . fst) $ binaryOperation op
        where
            failure (lDt, lPr) (rDt, rPr) = do
                unless (TypeError == lDt || TypeError == rDt) $ tellSError posn (BinaryTypes op (lDt,rDt))
                return (TypeError, lPr && rPr)
            success (_, lPr) (_, rPr) (_, dt) = return (dt, lPr && rPr)

    ExpUnary (Lex op _) expr  -> do
        oInfo@(oDt, oPr) <- checkExpression expr
        maybe (failure oInfo) (success oInfo) $ find ((oDt ==) . fst) $  unaryOperation op
        where
            failure (oDt, oPr) = do
                unless (TypeError == oDt) $ tellSError posn (UnaryTypes op oDt)
                return (TypeError, oPr)
            success (_, oPr) (_, dt) = return (dt, oPr)
