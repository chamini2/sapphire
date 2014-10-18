module Language.Sapphire.Program
    ( Program(..)

    , module Language.Sapphire.DataType
    , module Language.Sapphire.Declaration
    , module Language.Sapphire.Expression
    , module Language.Sapphire.Identifier
    , module Language.Sapphire.Lexeme
    , module Language.Sapphire.Statement
    ) where


import           Language.Sapphire.DataType
import           Language.Sapphire.Declaration
import           Language.Sapphire.Expression
import           Language.Sapphire.Identifier
import           Language.Sapphire.Lexeme
import           Language.Sapphire.Statement

--------------------------------------------------------------------------------

newtype Program = Program StBlock
