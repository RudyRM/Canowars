module InGame (gameDisplay) where

import Graphics.Gloss
import Types (CannonType(..), ScenarioType (..))

-- Se muestran unos humildes puntos ya que no pude poner una foto :D
drawCannon :: CannonType -> Float -> Float -> Picture
drawCannon Comunista      x y = translate x y (color red (circleSolid 20))
drawCannon Nazi x y = translate x y (color black (circleSolid 20))
drawCannon Vaticano  x y = translate x y (color yellow (circleSolid 20))
drawCannon EEUU   x y = translate x y (color blue (circleSolid 20))

-- Se dibuja la línea divisoria en el centro de la pantalla
drawDivider :: Picture
drawDivider = translate 0 0 (color black (line [(0, -600), (0, 600)]))

gameDisplay :: CannonType -> CannonType -> ScenarioType -> Picture
gameDisplay p1Cannon p2Cannon escenario = 
  pictures [
      drawCannon p1Cannon (-400) (-300),
      drawCannon p2Cannon 400 (-300),
      drawDivider
  ]

