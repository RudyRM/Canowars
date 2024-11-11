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
    --lado :: Int,
    spriteProyectil :: Picture
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
} deriving (Show, Eq)

