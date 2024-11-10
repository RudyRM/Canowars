module Types where
import Graphics.Gloss (Picture)

data CannonType = Comunista | Nazi | Vaticano | EEUU deriving (Show, Eq)
data Turno = Jugador1 | Jugador2 deriving (Show, Eq)
data ScenarioType = TorresGemelas | MurallaChina | MuroDeBerlin | TorresDelPaine deriving (Show)
data Proyectil = Proyectil {
    dañoProyectil :: Int,
    posXProyectil :: Float,
    posYProyectil :: Float,
    spriteProyectil :: Picture,
    anguloProyectil :: Float  -- Almacena el ángulo inicial del proyectil
} deriving (Show, Eq)
data Jugador = Jugador { 
    vida :: Int
    , daño :: Int
    , crítico :: Float
    , combustible :: Int
    , angulo :: Float
    , posX :: Float
    , sprite :: Picture
    , proyectil :: Maybe Proyectil
} deriving (Show, Eq)

