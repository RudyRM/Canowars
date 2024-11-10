module Types where
import Graphics.Gloss (Picture)

data CannonType = Comunista | Nazi | Vaticano | EEUU deriving (Show, Eq)
data Player = Player1 | Player2 deriving (Show, Eq)
data ScenarioType = TorresGemelas | MurallaChina | MuroDeBerlin | TorresDelPaine deriving (Show)
data Jugador = Jugador { vida :: Int
                        , daño :: Int
                        , crítico :: Float
                        , combustible :: Int
                        , angulo :: Float
                        , posX :: Float
                        , sprite :: Picture
                        , proyectil :: Maybe Proyectil
                        } deriving (Show, Eq)


data Proyectil = Proyectil { 
    posXProyectil :: Float,
    posYProyectil :: Float,
    velocidadX :: Float,
    velocidadY :: Float,
    dañoProyectil :: Int,
    spriteProyectil :: Picture 
} deriving (Show, Eq)

