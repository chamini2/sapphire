data Access = VariableAccess (Lexeme Identifier)
            | ArrayAccess    (Lexeme Access)     (Lexeme Expression)
            | StructAccess   (Lexeme Access)     (Lexeme Identifier)
            deriving (Eq)

data AccessHistory = HistoryArray  (Lexeme Expression)
                   | HistoryStruct (Lexeme Identifier)

type Thread = [AccessHistory]

type Zipper = (Access, Thread)

{-
    deriving the Zipper type

    acc = (var x iden) + (arr x acc x expr) + (str x acc x iden)
    expr = A
    iden = B

    acc = (1 x B) + (1 x acc x A) + (1 x acc x B)
    acc = (1 x B) + (acc x A)     + (acc x B)

    acc' = A + B
-}

getVariableAccess :: Lexeme Access -> Zipper
getVariableAccess (Lex acc _) = case acc of
    VariableAccess idenL -> idenL
    ArrayAccess  accL _  -> getVariableAccess accL
    StructAccess accL _  -> getVariableAccess accL
