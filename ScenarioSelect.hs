module ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario) where

import Graphics.Gloss
import System.Random (randomRIO) -- Importa randomRIO para la selección aleatoria
import Types (Player(..), ScenarioType(..)) -- Importa Player y ScenarioType

-- Dibuja la pantalla de selección de escenarios
drawScenarioSelect :: Picture
drawScenarioSelect = Pictures
  [ Translate (-300) 100 (Scale 0.5 0.5 (Text "Seleccione el escenario"))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text "Presiona '1' para seleccionar escenario Torres Gemelas"))
  , Translate (-300) 0   (Scale 0.4 0.4 (Text "Presiona '2' para seleccionar escenario Muralla China"))
  , Translate (-300) (-50) (Scale 0.4 0.4 (Text "Presiona '3' para seleccionar escenario Muro de Berlin"))
  , Translate (-300) (-100) (Scale 0.4 0.4 (Text "Presiona '4' para seleccionar escenario Torres del paine"))
  , Translate (-300) (-150) (Scale 0.4 0.4 (Text "Presiona '5' para seleccionar escenario Torre Eiffel"))
  , Translate (-300) (-200) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]

-- Dibuja la pantalla de selección de escenario para un jugador
drawScenarioSelectionScreen :: Player -> Picture
drawScenarioSelectionScreen player = Pictures
  [ Translate (-300) 150 (Scale 0.5 0.5 (Text ("Seleccione su escenario - " ++ show player)))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text "Presiona '1' para Escenario Torres Gemelas"))
  , Translate (-300) 0  (Scale 0.4 0.4 (Text "Presiona '2' para Escenario Muralla China"))
  , Translate (-300) (-50) (Scale 0.4 0.4 (Text "Presiona '3' para Escenario Muro de Berlin"))
  , Translate (-300) (-100) (Scale 0.4 0.4 (Text "Presiona '4' para Escenario Torres del paine"))
  , Translate (-300) (-150) (Scale 0.4 0.4 (Text "Presiona '5' para Escenario Torre Eiffel"))
  , Translate (-300) (-200) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]

-- Dibuja un resumen de los escenarios seleccionados por los jugadores
drawScenarioSummary :: ScenarioType -> ScenarioType -> Picture
drawScenarioSummary p1Scenario p2Scenario = Pictures
  [ Translate (-300) 100 (Scale 0.5 0.5 (Text "Resumen de Selección de Escenarios"))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text $ "Jugador 1 seleccionó: " ++ show p1Scenario))
  , Translate (-300) 0 (Scale 0.4 0.4 (Text $ "Jugador 2 seleccionó: " ++ show p2Scenario))
  , Translate (-300) (-50) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]

-- Función para seleccionar aleatoriamente uno de los dos escenarios elegidos
randomSelectScenario :: ScenarioType -> ScenarioType -> IO ScenarioType
randomSelectScenario p1Scenario p2Scenario = do
  index <- randomRIO (0, 1) :: IO Int
  return $ if index == 0 then p1Scenario else p2Scenario
