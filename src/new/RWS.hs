{-# LANGUAGE RankNTypes #-}
module RWS where

import           Error
import           Program
import           Symbol

import           Control.Monad.RWS (RWS, tell)
import           Data.Function     (on)
import qualified Data.Map          as DM (Map, fromList)
import           Data.Sequence     as DS (Seq, empty, singleton)

--------------------------------------------------------------------------------
-- Reader

data SappReader = SappReader
    { flags :: Seq Flag
    , arch  :: Architecture
    }

data Flag = OutputFile FilePath | SupressWarnings | AllWarnings
    deriving (Eq)

data Architecture = Arch
    { archName :: String
    , widths   :: DM.Map DataType Width
    } deriving (Show)

----------------------------------------
-- Instances

instance Eq Architecture where
    (==) = (==) `on` archName

----------------------------------------
-- Initial

initialReader :: SappReader
initialReader = SappReader
    { flags  = empty
    , arch   = defaultArchitecture
    }

defaultArchitecture :: Architecture
defaultArchitecture = Arch
    { archName = "mips"
    , widths = DM.fromList
        [ (Int     , 32)
        , (Float   , 32)
        , (Char    , 8)
        , (Bool    , 8)
        --, (Pointer , 32)
        ]
    }

--------------------------------------------------------------------------------
-- Writer

type SappWriter = Seq Error

--------------------------------------------------------------------------------
-- Error reporting

tellLError :: forall r s. Position -> LexerError -> RWS r SappWriter s ()
tellLError posn err = tell (singleton $ LError posn err)

tellPError :: forall r s. Position -> ParseError -> RWS r SappWriter s ()
tellPError posn err = tell (singleton $ PError posn err)

tellSError :: forall r s. Position -> StaticError -> RWS r SappWriter s ()
tellSError posn err = tell (singleton $ SError posn err)

tellWarn :: forall r s. Position -> Warning -> RWS r SappWriter s ()
tellWarn posn err = tell (singleton $ Warn posn err)
