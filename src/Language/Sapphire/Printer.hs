module Language.Sapphire.Printer where

import           Control.Monad.Identity hiding (forM_, mapM_)
import           Control.Monad.State    hiding (forM_, mapM_)
import           Control.Monad.Writer   hiding (forM_, mapM_)

data PrintState = PrintState { tabs :: Int }

initialPrintState :: PrintState
initialPrintState = PrintState 0

type Printer a = StateT PrintState (WriterT (Seq String) Identity) a

printStatement :: Statement -> Printer ()
printStatement _ = undefined
