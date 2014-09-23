module Error where

import           Program
import           SymbolTable

import           Data.Function (on)
import           Data.Sequence (Seq)

--------------------------------------------------------------------------------

data Error
    = LError Position LexerError
    | PError Position ParseError
    | SError Position StaticError
    | Warn   Position Warning

instance Show Error where
    show sappError = case sappError of
        LError p e -> "Lexer error "   ++ show p ++ ": \n\t" ++ show e ++ "\n"
        PError p e -> "Parsing error " ++ show p ++ ": \n\t" ++ show e ++ "\n"
        SError p e -> "Static error "  ++ show p ++ ": \n\t" ++ show e ++ "\n"
        Warn   p w -> "Warning "       ++ show p ++ ": \n\t" ++ show w ++ "\n"

instance Eq Error where
    (==) = (==) `on` errorPos

instance Ord Error where
    compare = compare `on` errorPos

----------------------------------------

data LexerError
    = LexerError     String
    | UnexpectedChar Char
    | StringError    String

instance Show LexerError where
    show lError = case lError of
        LexerError msg   -> msg
        UnexpectedChar c -> "unexpected character '" ++ [c] ++ "'"
        StringError str  -> "missing matching quotation mark for string " ++ show str

----------------------------------------

data ParseError
    = ParseError      String
    | UnexpectedToken String
    -- Identifiers
    | TypeIdInsteadOfVarId String
    | VarIdInsteadOfTypeId String
    -- Lists
    | VariableListComma
    | FieldListComma
    | ParameterListComma
    -- Statements
    | AssignmentMissingExpression
    | AssignmentMissingAccess
    | VariableDefinitionMissingColon

instance Show ParseError where
    show pError = case pError of
        ParseError msg      -> msg
        UnexpectedToken tok -> "unexpected token '" ++ show tok ++ "'"
        -- Identifiers
        TypeIdInsteadOfVarId idn -> "identifier '" ++ idn ++ "' must start with a lowercase letter"
        VarIdInsteadOfTypeId idn -> "type '" ++ idn ++ "' must start with an uppercase letter"
        -- Lists
        VariableListComma      -> "variables must be separated with commas"
        FieldListComma         -> "fields must be separated with commas"
        ParameterListComma     -> "parameters must be separated with commas"
        -- Statements
        AssignmentMissingExpression    -> "assignment missing expression"
        AssignmentMissingAccess        -> "assignment missing variable"
        VariableDefinitionMissingColon -> "variable definition missing colon"

----------------------------------------

data StaticError
    = StaticError String
    -- Variables
--    | VariableNotInitialized Identifier
    | InvalidAssignType      Identifier DataType DataType
--    | ArraySizeDataType      Expression DataType
--    | ImpureArraySize        Expression
    -- Types
    | TypeAlreadyDefined    Identifier Position
    | TypeIsLanguageDefined Identifier
    | UndefinedType         Identifier
    | RecursiveStruct       Identifier Identifier
    | TypeNotYetDefined     Identifier Identifier Identifier Position
    | AccessNonArray        Identifier DataType
    | IndexDataType         Expression DataType
    | AccessNonStruct       Identifier DataType
    | StructNoField         DataType   Identifier
    | ReadNonReadable       DataType   Identifier
    | CaseNonCaseable       DataType
    -- Functions
--    | FunctionNotDefined       Identifier
    | ProcedureInExpression    Identifier
    | FunctionAsStatement      Identifier
--    | UsedNotImplemented       Identifier
--    | ImpInDefScope            Identifier Position
--    | AlreadyImplemented       Identifier Position
--    | LanguageImplemented      Identifier
    | FunctionArguments        Identifier (Seq DataType) (Seq DataType)
    | FunctionAlreadyDefined   Identifier Position
--    | LanguageFunctionRedefine Identifier
--    | NoReturn                 Identifier
    -- Statements
    | ConditionDataType DataType
    | PrintNonString    DataType
    | CaseWhenDataType  DataType DataType
    | ForInDataType     DataType
    | BreakOutsideLoop
    | ContinueOutsideLoop
    | ReturnInProcedure          DataType Identifier
    | ReturnType        DataType DataType Identifier
    -- Operators
    | BinaryTypes Binary (DataType, DataType)
    | UnaryTypes  Unary  DataType
    -- General
    | WrongCategory   Identifier SymbolCategory SymbolCategory
    | NotDefined      Identifier
    | AlreadyDeclared Identifier Position
    deriving Show

--instance Show StaticError where
--    show sError = case sError of
--        StaticError msg -> msg
--        -- Variables
--        VariableNotInitialized var       -> "variable '" ++ var ++ "' may not have been initialized"
--        InvalidAssignType      var vt et -> "cannot assign expression of type '" ++ show et ++ "' to variable '" ++ var ++ "' of type '" ++ show vt ++ "'"
--        VariableNonArray       var dt    -> "variable '" ++ var ++ "' of type '" ++ show dt ++ "' is being used as an array"
--        VariableNonStruct      var dt    -> "variable '" ++ var ++ "' of type '" ++ show dt ++ "' is being used as a structure"
--        StructNoField          str fn    -> "structure '" ++ str ++ "' has no field named '" ++ fn ++ "'"
        --ReadNonReadable        dt  idn   -> "variable '" ++ idn ++ "' of type '" ++ show dt ++ "' cannot be used for the 'read' statement"
        --CaseNonCaseable        dt  idn   -> "variable '" ++ idn ++ "' of type '" ++ show dt ++ "' cannot be used as the expression of a 'case' statement"
--        IndexDataType          expr dt   -> "index expression '" ++ showIndex expr ++ "' is of type '" ++ show dt ++ "', but 'Int' was expected"
--        ArraySizeDataType      expr dt   -> "array size expression '" ++ showIndex expr ++ "' is of type '" ++ show dt ++ "', but 'Int' was expected"
--        --ImpureArraySize        expr      -> "array size expression '" ++ showIndex expr ++ "' is 'impure'"
--        ImpureArraySize        expr      -> "array size expression '" ++ showIndex expr ++ "' must be an 'Int' literal"
--        -- Types
--        TypeAlreadyDefined   tname p -> "type '" ++ tname ++ "' has already been defined " ++ show p
--        LanguageTypeRedefine tname   -> "cannot redefine a language defined type '" ++ tname ++ "'"
--        UndefinedType        tname   -> "type '" ++ tname ++ "' has not been defined"
        --RecursiveStruct      tname field -> "field '" ++ field ++ "' in type '" ++ tname ++ "' creates an infinite recursion in the structure"
        --TypeNotYetDefined tname field struct posn -> "field '" ++ field ++ "' in type '" ++ tname ++ "' can't use the type '" ++ struct ++ "' because it is defined '" ++ posn ++ "' (must be defined before usage inside structures)"
--        -- Functions
--        FunctionNotDefined    fname     -> "must define function '" ++ fname ++ "' before implementing it"
--        ProcedureInExpression fname     -> "cannot use procedure '" ++ fname ++ "' inside an expression"
--        FunctionAsStatement   fname     -> "cannot use function '" ++ fname ++ "' as a statement"
--        UsedNotImplemented    fname     -> "function '" ++ fname ++ "' is used but never implemented"
--        ImpInDefScope         fname p   -> "must implement function '" ++ fname ++ "' in same scope that it is defined, " ++ show p
--        AlreadyImplemented    fname p   -> "function '" ++ fname ++ "' has already been implemented " ++ show p
--        LanguageImplemented   fname     -> "cannot reimplement language implemented function '"++ fname ++ "'"
--        FunctionArguments     fname e g -> "function '" ++ fname ++ "' expects arguments (" ++ showSign e ++ "), but was given (" ++ showSign g ++ ")"
--            where
--                showSign = intercalate ", " . map show . toList
--        FunctionAlreadyDefined   fname p -> "function '" ++ fname ++ "' has already been defined " ++ show p
--        LanguageFunctionRedefine fname   -> "cannot redefine a language defined function '" ++ fname ++ "'"
--        NoReturn                 fname   -> "function '" ++ fname ++ "' does not have a return statement"
--        -- Statements
--        ConditionDataType dt    -> "condition must be of type 'Bool', but it is of type '" ++ show dt ++ "'"
--        CaseWhenDataType e g    -> "case has expression of type '" ++ show e ++ "' but when has expression of type '" ++ show g ++ "'"
--        ForInDataType dt        -> "for statement must iterate over expression of type 'Range', but it is of type '" ++ show dt ++ "'"
--        BreakOutsideLoop        -> "break statement not within loop"
--        ContinueOutsideLoop     -> "continue statement not within loop"
--        ReturnProcedure g fname -> "cannot return '" ++ show g ++ "' in procedure '" ++ fname ++ "'"
--        ReturnType e g fname    -> "expected return type '" ++ show e ++ "' for function '" ++ fname ++ "', but got type '" ++ show g ++ "'"
--        -- Operators
--        UnaryTypes  op dt      -> "operator '" ++ show op ++ "' does not work with operand (" ++ show dt ++ ")"
--        BinaryTypes op (dl,dr) -> "operator '" ++ show op ++ "' does not work with operands (" ++ show dl ++ ", " ++ show dr ++ ")"
--        -- General
--        WrongCategory iden e g -> "using '" ++ iden ++ "' as if it is a " ++ show e ++ ", but it is a " ++ show g
--        NotDefined  iden       -> "identifier '" ++ iden ++ "' has not been defined"
--        AlreadyDeclared var p  -> "identifier '" ++ var ++ "' has already been declared " ++ show p

----------------------------------------

data Warning
    = DefinedNotUsed        Identifier
    | DefinedNotImplemented Identifier
    | Warning               String

instance Show Warning where
    show cWarn = case cWarn of
        DefinedNotUsed iden         -> "identifier '" ++ iden ++ "' is defined but never used"
        DefinedNotImplemented fname -> "function '" ++ fname ++ "' is defined but never implemented"
        Warning msg                 -> msg

--------------------------------------------------------------------------------

errorPos :: Error -> Position
errorPos err = case err of
    LError p _ -> p
    PError p _ -> p
    SError p _ -> p
    Warn   p _ -> p
