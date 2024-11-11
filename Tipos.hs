module Tipos where
import Graphics.Gloss (Picture)
import System.Random (StdGen)

data Turno = Jugador1 | Jugador2 deriving (Show, Eq)

data Proyectil = Proyectil {
    dañoProyectil :: Int,
    posIniXProyectil :: Float,
    anguloIniProyectil :: Float,
    posXProyectil :: Float,
    posYProyectil :: Float,
    spriteProyectil :: Picture,
    ladoProyectil :: Float
} deriving (Show, Eq)

data Jugador = Jugador { 
    vida :: Int
    , combustibleJugador :: Int
    , anguloJugador :: Float
    , posXJugador :: Float
    , spriteJugador :: Picture
    , semillaDesvioAngulo :: StdGen
    , semillaCritico :: StdGen
    , textoJugador :: Picture
    , ladoJugador :: Float
} deriving (Show, Eq)

data Punto2D a = Punto2D a a deriving Show

instance Functor Punto2D where
    fmap f (Punto2D x y) = Punto2D (f x) (f y)
