module ScenarioSelect (drawScenarioSelect, drawScenarioSelectionScreen, drawScenarioSummary, randomSelectScenario) where

import Graphics.Gloss
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO) -- Importa randomRIO para la selección aleatoria
import Types (Turno(..), ScenarioType(..)) -- Importa Player y ScenarioType

-- Dibuja la pantalla de selección de escenarios
drawScenarioSelect :: Picture
drawScenarioSelect = Pictures
  [ 
    Translate 0 0 (Scale 0.675 0.675 fondo)
  , Translate (-200) 50 (Scale 0.1 0.1 mapa_1)
  , Translate 200 50 (Scale 0.1 0.1 mapa_2)
  , Translate (-200) (-200) (Scale 0.1 0.1 mapa_3)
  , Translate 200 (-200) (Scale 0.1 0.1 mapa_4)
  , Translate (-205) (-30) (Scale 0.275 0.275 num_1)
  , Translate 195 (-30) (Scale 0.275 0.275 num_2)
  , Translate (-205) (-280) (Scale 0.275 0.275 num_3)
  , Translate 195 (-280) (Scale 0.275 0.275 num_4)
  ]

fondo :: Picture
fondo = unsafePerformIO $ loadBMP "assets/fondos/War3/Pale/War3.bmp"

mapa_1 :: Picture
mapa_1 = unsafePerformIO $ loadBMP "assets/fondos/War1/Bright/War.bmp"
mapa_2 :: Picture
mapa_2 = unsafePerformIO $ loadBMP "assets/fondos/War2/Bright/War2.bmp"
mapa_3 :: Picture
mapa_3 = unsafePerformIO $ loadBMP "assets/fondos/War3/Bright/War3.bmp"
mapa_4 :: Picture
mapa_4 = unsafePerformIO $ loadBMP "assets/fondos/War4/Bright/War4.bmp"

num_1 :: Picture
num_1 = unsafePerformIO $ loadBMP "assets/ui/1.bmp"
num_2 :: Picture
num_2 = unsafePerformIO $ loadBMP "assets/ui/2.bmp"
num_3 :: Picture
num_3 = unsafePerformIO $ loadBMP "assets/ui/3.bmp"
num_4 :: Picture
num_4 = unsafePerformIO $ loadBMP "assets/ui/4.bmp"

-- Dibuja la pantalla de selección de escenario para un jugador
drawScenarioSelectionScreen :: Turno -> Picture
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
