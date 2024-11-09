
import Graphics.Gloss.Interface.Pure.Game
import Menu
import NextScreen
import InGame (gameDisplay)
import CanonSelect (drawCanonSelectionScreen, drawSelectionSummary)
import ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario)
import Types (CannonType(..), Player(..), ScenarioType(..))

data GameState = Menu 
               | CanonSelect Player CannonType
               | ScenarioSelect CannonType CannonType
               | InGame CannonType CannonType ScenarioType -- Agregar ScenarioType para el escenario final

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
draw Menu                          = drawMenu
--draw (CanonSelect currentPlayer _ _) = drawCanonSelectionScreen currentPlayer
--draw (ScenarioSelect currentPlayer _ _) =      currentPlayer
--draw (NextScreen p1Cannon p2Cannon scenario) = drawSelectionSummary p1Cannon p2Cannon <> drawScenarioSummary scenario scenario

draw (CanonSelect _ _) = drawCanonSelectionScreen
draw (ScenarioSelect _ _) = drawScenarioSelect
draw (InGame p1Cannon p2Cannon escenario) = gameDisplay p1Cannon p2Cannon escenario

handleInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = CanonSelect Player1 Nazi
-- Volver al menú desde la selección de cañones o escenarios
handleInput (EventKey (Char 'q') Down _ _) (CanonSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (ScenarioSelect _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (InGame _ _ _) = Menu
-- Selección de cañonesa
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 Nazi
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 Comunista
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 Vaticano
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player1 _) = CanonSelect Player2 EEUU
-- Cuando el jugador 2 selecciona su cañón, pasa a la selección de escenario
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player2 p1Cannon) = ScenarioSelect p1Cannon Nazi
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player2 p1Cannon) = ScenarioSelect p1Cannon Comunista
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player2 p1Cannon) = ScenarioSelect p1Cannon Vaticano
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player2 p1Cannon) = ScenarioSelect p1Cannon EEUU
-- Selección de escenarios
handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon TorresGemelas
handleInput (EventKey (Char '2') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon MurallaChina
handleInput (EventKey (Char '3') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon MuroDeBerlin
handleInput (EventKey (Char '4') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon TorresDelPaine
handleInput (EventKey (Char '5') Down _ _) (ScenarioSelect p1Cannon p2Cannon) = InGame p1Cannon p2Cannon TorreEiffel

-- Al seleccionar el escenario para el jugador 2, selecciona aleatoriamente entre los dos escenarios elegidos
-- handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect Player2 (Just p1Scenario) _) = NextScreen p1Scenario p1Scenario undefined -- placeholder
handleInput _ state = state

update :: Float -> GameState -> GameState
update _ state = state
