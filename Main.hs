
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Menu
import Data.Maybe (isNothing, fromJust)
import NextScreen
import System.IO.Unsafe (unsafePerformIO)
import InGame (gameDisplay)
import CanonSelect (drawCanonSelectionScreen)
import ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario)
import Types (CannonType(..), Player(..), ScenarioType(..), Jugador(..), Proyectil(..))
import Acciones(moveTank)
import Debug.Trace

data GameState = Menu 
                | CanonSelect Player (Maybe Jugador)
                | ScenarioSelect Jugador Jugador
                | InGame Jugador Jugador Picture Bool 

main :: IO ()
main = do
  play
    (InWindow "Pantalla" (1280, 720) (100, 100))
    white
    60
    Menu
    draw
    handleInput
    update

draw :: GameState -> Picture
draw Menu = drawMenu
draw (CanonSelect _ _) = drawCanonSelectionScreen
draw (ScenarioSelect _ _) = drawScenarioSelect
draw (InGame p1Cannon p2Cannon escenario turno) = gameDisplay p1Cannon p2Cannon escenario

handleInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = CanonSelect Player1 Nothing
-- Volver al menú desde la selección de cañones o escenarios
handleInput (EventKey (Char 'q') Down _ _) (CanonSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (ScenarioSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (InGame _ _ _ _) = Menu
-- Selección de cañonesa
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp")})
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp")})
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp")})
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.9, proyectil = Nothing, posX = -350, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp")})
-- Cuando el jugador 2 selecciona su cañón, pasa a la selección de escenario
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = scale (-1) 1 $ (unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp")})
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = scale (-1) 1 $ (unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp")})
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = scale (-1) 1 $ (unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp")})
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = -0.9, proyectil = Nothing, posX = 350, sprite = scale (-1) 1 $ (unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp")})
-- Selección de escenarios
handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War1/Bright/War.bmp") True
handleInput (EventKey (Char '2') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War2/Bright/War2.bmp") True
handleInput (EventKey (Char '3') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War3/Bright/War3.bmp") True
handleInput (EventKey (Char '4') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War4/Bright/War4.bmp") True

-- Controles del tanque 1 (cuando el turno es True)
handleInput (EventKey (Char 'w') Down _ _) (InGame p1Cannon p2Cannon escenario True) =  
  let nuevoP1Cannon = p1Cannon { angulo = angulo p1Cannon + 0.01 }
  in InGame nuevoP1Cannon p2Cannon escenario True

handleInput (EventKey (Char 's') Down _ _) (InGame p1Cannon p2Cannon escenario True) =  
  let nuevoP1Cannon = p1Cannon { angulo = angulo p1Cannon - 0.01 }
  in InGame nuevoP1Cannon p2Cannon escenario True

handleInput (EventKey (Char 'a') Down _ _) (InGame p1Cannon p2Cannon escenario True) =  
  let nuevoP1Cannon = moveTank p1Cannon (-5)
  in InGame nuevoP1Cannon p2Cannon escenario True

handleInput (EventKey (Char 'd') Down _ _) (InGame p1Cannon p2Cannon escenario True) =  
  let nuevoP1Cannon = moveTank p1Cannon 5
  in InGame nuevoP1Cannon p2Cannon escenario True

-- Disparo del tanque 1 (cuando es su turno)
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (InGame p1Cannon p2Cannon escenario True) =
  let nuevoP1Cannon = p1Cannon { combustible = 80 } -- Solo actualiza el combustible del tanque 1
  in InGame nuevoP1Cannon p2Cannon escenario False -- Cambia el turno al tanque 2


-- Controles del tanque 2 (cuando el turno es False)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) (InGame p1Cannon p2Cannon escenario False) =  
  let nuevoP2Cannon = p2Cannon { angulo = angulo p2Cannon + 0.01 }
  in InGame p1Cannon nuevoP2Cannon escenario False

handleInput (EventKey (SpecialKey KeyUp) Down _ _) (InGame p1Cannon p2Cannon escenario False) =  
  let nuevoP2Cannon = p2Cannon { angulo = angulo p2Cannon - 0.01 }
  in InGame p1Cannon nuevoP2Cannon escenario False

handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (InGame p1Cannon p2Cannon escenario False) =  
  let nuevoP2Cannon = moveTank p2Cannon (-5)
  in InGame p1Cannon nuevoP2Cannon escenario False

handleInput (EventKey (SpecialKey KeyRight) Down _ _) (InGame p1Cannon p2Cannon escenario False) =  
  let nuevoP2Cannon = moveTank p2Cannon 5
  in InGame p1Cannon nuevoP2Cannon escenario False

-- Disparo del tanque 2 (cuando es su turno)
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (InGame p1Cannon p2Cannon escenario False) = 
  let nuevoP2Cannon = p2Cannon { combustible = 80 } -- Solo actualiza el combustible del tanque 2
  in InGame p1Cannon nuevoP2Cannon escenario True -- Cambia el turno al tanque 1


-- Caso por defecto para otros eventos
handleInput _ state = state

update :: Float -> GameState -> GameState
update _ state = state
