{-|
    Tabla de simbolos basada en una tabla de hash con Strings como 
    claves y un Data.Sequence de InfoSim como valores         
-}
module TablaSim (
    -- * Tipos exportados
    TablaSim,
    InfoSim  (..),
    BrainVal (..),
    Scope,
    -- * Funciones exportadas
    tablaVacia,
    insertar,
    eliminarVariable,
    buscarInfoSim,
    actualizar
)
where

import Data.Sequence as DS
import qualified Data.Map as DM

import Cinta
import SinBrainiac (VarName, Tipo)

data TablaSim = TablaSim (DM.Map VarName (Seq InfoSim))
    deriving (Show)

data InfoSim = InfoSim {
    tipo    :: Tipo,
    scope   :: Scope,
    valor   :: BrainVal,
    ocupado :: Bool
} deriving (Show)

type Scope = Int

data BrainVal = ValN Int 
              | ValB Bool
              | ValC Cinta 
              | Null
              deriving (Eq)

instance Show BrainVal where 
    show (ValN v) = show v
    show (ValB v) = show v
    show (ValC v) = show v
    show Null     = "Null"

{-| 
    Crear una tabla de simbolos vacia    
-}
tablaVacia :: TablaSim 
tablaVacia = TablaSim (DM.empty)

{-|
    Agregar la variable v a la tabla de simbolos
-}
insertar :: VarName -> InfoSim -> TablaSim -> TablaSim 
insertar vn info (TablaSim m) = TablaSim $ DM.alter f vn m
    where f Nothing  = Just $ DS.singleton info
          f (Just x) = Just $ info <| x

{-|
    Eliminar la variable v del scope mas interno
-}
eliminarVariable :: VarName -> TablaSim -> TablaSim
eliminarVariable id (TablaSim m) = TablaSim $ DM.alter f id m
    where f Nothing  = Just DS.empty
          f (Just s) = Just $ DS.drop 1 s

{-|
    Buscar la informacion relacionada con una variable en la tabla de simbolos
-}
buscarInfoSim :: VarName -> TablaSim -> Maybe InfoSim 
buscarInfoSim id (TablaSim m) = do
    is <- DM.lookup id m
    case viewl is of
        EmptyL    -> Nothing
        info :< _ -> Just info

{-|
    Actualizar el valor de una variable, solo si la variable esta 
    en la tabla de simbolos
 -}
actualizar :: VarName -> BrainVal -> TablaSim -> TablaSim 
actualizar id vn (TablaSim m) = TablaSim $ DM.alter f id m
    where f (Just is) =
            case viewl is of 
                i :< iss -> Just $ i { valor = vn } <| iss
