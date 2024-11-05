module Main where

import Graphics.Gloss.Interface.Pure.Game
import Menu
import NextScreen
import CanonSelect (drawCanonSelect, drawCanonSelectionScreen, drawSelectionSummary)
import ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario)
import Types (CannonType(..), Player(..), ScenarioType(..))

data GameState = Menu 
               | CanonSelect Player (Maybe CannonType) (Maybe CannonType)
               | ScenarioSelect Player (Maybe ScenarioType) (Maybe ScenarioType)
               | NextScreen CannonType CannonType ScenarioType -- Agregar ScenarioType para el escenario final

main :: IO ()
main = do
  play
    (InWindow "Pantalla" (800, 600) (100, 100))
    white
    60
    Menu
    draw
    handleInput
    update

draw :: GameState -> Picture
draw Menu                          = drawMenu
draw (CanonSelect currentPlayer _ _) = drawCanonSelectionScreen currentPlayer
draw (ScenarioSelect currentPlayer _ _) = drawScenarioSelectionScreen currentPlayer
draw (NextScreen p1Cannon p2Cannon scenario) = drawSelectionSummary p1Cannon p2Cannon <> drawScenarioSummary scenario scenario

handleInput :: Event -> GameState -> GameState
-- Transición al modo de selección de cañones desde el menú
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = CanonSelect Player1 Nothing Nothing
-- Volver al menú desde la selección de cañones o escenarios
handleInput (EventKey (Char 'q') Down _ _) (CanonSelect _ _ _) = Menu
handleInput (EventKey (Char 'q') Down _ _) (ScenarioSelect _ _ _) = Menu
-- Selección de cañones
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just USA) p2Cannon
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Italiano) p2Cannon
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Frances) p2Cannon
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Aleman) p2Cannon
handleInput (EventKey (Char '5') Down _ _) (CanonSelect Player1 _ p2Cannon) = CanonSelect Player2 (Just Chileno) p2Cannon
-- Cuando el jugador 2 selecciona su cañón, pasa a la selección de escenario
handleInput (EventKey (Char '1') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = ScenarioSelect Player1 Nothing Nothing
handleInput (EventKey (Char '2') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = ScenarioSelect Player1 Nothing Nothing
handleInput (EventKey (Char '3') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = ScenarioSelect Player1 Nothing Nothing
handleInput (EventKey (Char '4') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = ScenarioSelect Player1 Nothing Nothing
handleInput (EventKey (Char '5') Down _ _) (CanonSelect Player2 (Just p1Cannon) _) = ScenarioSelect Player1 Nothing Nothing
-- Selección de escenarios
handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect Player1 _ p2Scenario) = ScenarioSelect Player2 (Just TorresGemelas) p2Scenario
handleInput (EventKey (Char '2') Down _ _) (ScenarioSelect Player1 _ p2Scenario) = ScenarioSelect Player2 (Just MurallaChina) p2Scenario
handleInput (EventKey (Char '3') Down _ _) (ScenarioSelect Player1 _ p2Scenario) = ScenarioSelect Player2 (Just MuroDeBerlin) p2Scenario
handleInput (EventKey (Char '4') Down _ _) (ScenarioSelect Player1 _ p2Scenario) = ScenarioSelect Player2 (Just TorresDelPaine) p2Scenario
handleInput (EventKey (Char '5') Down _ _) (ScenarioSelect Player1 _ p2Scenario) = ScenarioSelect Player2 (Just TorreEiffel) p2Scenario
-- Al seleccionar el escenario para el jugador 2, selecciona aleatoriamente entre los dos escenarios elegidos
-- handleInput (EventKey (Char '1') Down _ _) (ScenarioSelect Player2 (Just p1Scenario) _) = NextScreen p1Scenario p1Scenario undefined -- placeholder
handleInput _ state = state

update :: Float -> GameState -> GameState
update _ state = state
