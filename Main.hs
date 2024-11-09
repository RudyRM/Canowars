
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Menu
import NextScreen
import System.IO.Unsafe (unsafePerformIO)
import InGame (gameDisplay)
import CanonSelect (drawCanonSelectionScreen)
import ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario)
import Types (CannonType(..), Player(..), ScenarioType(..), Jugador(..))

data GameState = Menu 
                | CanonSelect Player (Maybe Jugador)
                | ScenarioSelect Jugador Jugador
                | InGame Jugador Jugador Picture

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
draw (InGame p1Cannon p2Cannon escenario) = gameDisplay p1Cannon p2Cannon escenario

handleInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = CanonSelect Player1 Nothing
-- Volver al menú desde la selección de cañones o escenarios
handleInput (EventKey (Char 'q') Down _ _) (CanonSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (ScenarioSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (InGame _ _ _) = Menu
-- Selección de cañonesa
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp")})
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp")})
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp")})
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 (Just Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp")})
-- Cuando el jugador 2 selecciona su cañón, pasa a la selección de escenario
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_nazi.bmp")})
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_comunista.bmp")})
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_vaticano.bmp")})
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player2 (Just p1Cannon)) = ScenarioSelect p1Cannon (Jugador { vida = 100, daño = 50, crítico = 0.25, combustible = 80, angulo = 0.0, sprite = (unsafePerformIO $ loadBMP "assets/tanques/tank_eeuu.bmp")})
-- Selección de escenarios
handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War1/Bright/War.bmp")
handleInput (EventKey (Char '2') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War2/Bright/Wa2.bmp")
handleInput (EventKey (Char '3') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War3/Bright/Wa3.bmp")
handleInput (EventKey (Char '4') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon (unsafePerformIO $ loadBMP "assets/fondos/War4/Bright/Wa4.bmp")

-- Al seleccionar el escenario para el jugador 2, selecciona aleatoriamente entre los dos escenarios elegidos
-- handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect Player2 (Just p1Scenario) _) = NextScreen p1Scenario p1Scenario undefined -- placeholder
handleInput _ state = state

update :: Float -> GameState -> GameState
update _ state = state
