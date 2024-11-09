module Types where

data CannonType = Comunista | Nazi | Vaticano | EEUU deriving (Show, Eq)
data Player = Player1 | Player2 deriving (Show, Eq)
data ScenarioType = TorresGemelas | MurallaChina | MuroDeBerlin | TorresDelPaine | TorreEiffel deriving(Show)
data Jugador = Jugador {canon :: CannonType} deriving (Show, Eq)