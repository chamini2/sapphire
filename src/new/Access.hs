{-# LANGUAGE DeriveDataTypeable #-}
module Access where

import           Lexeme

import qualified Data.Data     as DD (Data)
import           Data.Functor  ((<$))
import           Data.Maybe    (fromJust)
import qualified Data.Typeable as DT (Typeable)


data Access = VariableAccess (Lexeme Identifier)
            | ArrayAccess    (Lexeme Access)     (Lexeme Expression)
            | StructAccess   (Lexeme Access)     (Lexeme Identifier)
            deriving (Eq, Ord, DT.Typeable, DD.Data)

instance Show Access where
    show acc = case acc of
        VariableAccess idnL      -> lexInfo idnL
        ArrayAccess    accL indL -> show (lexInfo accL) ++ "[" ++ show (lexInfo indL) ++ "]"
        StructAccess   accL fldL -> show (lexInfo accL) ++ "." ++ lexInfo fldL

{-
 - deriving the AccessHistory type
 -
 - acc = (var x iden) + (arr x acc x expr) + (str x acc x iden)
 - expr = A
 - iden = B
 -
 - acc = (1 x B) + (1 x acc x A) + (1 x acc x B)
 - acc = (1 x B) + (acc x A)     + (acc x B)
 -
 - acc' = A + B
 -}

data AccessHistory = HistoryArray  (Lexeme Expression)
                   | HistoryStruct (Lexeme Identifier)
                   deriving (Show)

type Thread = [Lexeme AccessHistory]

type Zipper = (Lexeme Access, Thread)

----------------------------------------

focusAccess :: Lexeme Access -> Zipper
focusAccess accL = (accL, [])

defocusAccess :: Zipper -> Lexeme Access
defocusAccess (accL, _) = accL

inArrayAccess :: Zipper -> Maybe Zipper
inArrayAccess (histL@(Lex acc _), thrd) = case acc of
    ArrayAccess accL indexL -> Just (accL, (HistoryArray indexL <$ histL) : thrd)
    _                       -> Nothing

inStructAccess :: Zipper -> Maybe Zipper
inStructAccess (histL@(Lex acc _), thrd) = case acc of
    StructAccess accL fieldL -> Just (accL, (HistoryStruct fieldL <$ histL) : thrd)
    _                        -> Nothing

inAccess :: Zipper -> Maybe Zipper
inAccess zpp@(accL,_) = case lexInfo accL of
    VariableAccess _   -> Nothing
    ArrayAccess    _ _ -> inArrayAccess zpp
    StructAccess   _ _ -> inStructAccess zpp

backAccess :: Zipper -> Maybe Zipper
backAccess (accL, thrd) = case thrd of
    []                      -> Nothing
    hstL@(Lex hist _) : hstLs -> case hist of
        HistoryArray  indexL -> Just (ArrayAccess  accL indexL <$ hstL, hstLs)
        HistoryStruct fieldL -> Just (StructAccess accL fieldL <$ hstL, hstLs)

topAccess :: Zipper -> Zipper
topAccess zpp = case snd zpp of
    []     -> zpp
    _ : _ -> topAccess $ fromJust $ backAccess zpp

deepAccess :: Zipper -> Zipper
deepAccess zpp@(accL, _) = case lexInfo accL of
    VariableAccess _ -> zpp
    _                -> deepAccess $ fromJust $ inAccess zpp
