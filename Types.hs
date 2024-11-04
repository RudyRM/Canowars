module Types where

data CannonType = USA | Italiano | Frances | Aleman | Chileno deriving (Show, Eq)
data Player = Player1 | Player2 deriving (Show, Eq)
data ScenarioType = TorresGemelas | MurallaChina | MuroDeBerlin | TorresDelPaine | TorreEiffel