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
                        , sprite :: Picture
                        } deriving (Show, Eq)
