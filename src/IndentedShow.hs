{-# LANGUAGE RankNTypes #-}

module IndentedShow
    ( IndentedShow(ishow)
    , indentShow
    , runIndentedShow
    , string, stringL
    , line, indent
    ) where

import Control.Lens
import Control.Monad.State (State, execState)
import Data.Foldable (toList)
import Data.List (intercalate, foldl')

type Indentation = Int
type IndentedShowed = State PrettyShowState ()

data PrettyShowState
  = PrettyShowState
        { _ps_level :: Indentation
        , _ps_lines :: [(Indentation, String)]
        }

makeLenses ''PrettyShowState

class IndentedShow a where
    ishow :: a -> IndentedShowed

indentShow :: IndentedShow a => a -> String
indentShow = runIndentedShow "│ " "└ " . ishow

runIndentedShow :: String -- indentation string
                -> String -- end of indentation string
                -> IndentedShowed
                -> String
runIndentedShow dstr estr = views ps_lines showLines . flip execState initialST
  where
    showLines :: [(Indentation, String)] -> String
    showLines = intercalate "\n" . fmap indent . reverse . ends
      where
        ends :: [(Indentation, String)] -> [((Indentation, Indentation), String)]
        ends =  reverse . foldl' mark []
          where
            mark ls (ind, str) = (markInd ls ind, str) : ls
            markInd ls ind = case ls of
                [] -> (0, ind)
                ((lind, lend),_):_ -> let
                        prev = lind + lend
                        end = if ind > prev then ind - prev else 0
                    in (ind - end, end)
        indent :: ((Indentation, Indentation), String) -> String
        indent (ind, str) = tabs ind ++ str
        tabs :: (Indentation, Indentation) -> String
        tabs (ind, end) = concat $ replicate ind dstr ++ replicate end estr
    initialST :: PrettyShowState
    initialST = PrettyShowState { _ps_level = 0 , _ps_lines  = [(0, "")] }

----------------------------------------

string :: String -> IndentedShowed
string str = ps_lines._head._2 %= (++ str)

stringL :: Lens' s String -> s -> IndentedShowed
stringL lns = views lns string

line :: IndentedShowed
line = use ps_level >>= \n -> ps_lines %= cons (n, "")

raise :: IndentedShowed
raise = ps_level += 1

lower :: IndentedShowed
lower = ps_level -= 1

indent :: IndentedShowed -> IndentedShowed
indent run = raise >> line >> run >> lower
