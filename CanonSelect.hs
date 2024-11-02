module CanonSelect (drawCanonSelect, drawCanonSelectionScreen, drawSelectionSummary) where

import Graphics.Gloss
import Types (Player(..), CannonType(..)) -- Importa Player y CannonType

drawCanonSelect :: Picture
drawCanonSelect = Pictures
  [ Translate (-300) 100 (Scale 0.5 0.5 (Text "Seleccione su tipo de canon"))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text "Presiona '1' para seleccionar canon de USA"))
  , Translate (-300) 0   (Scale 0.4 0.4 (Text "Presiona '2' para seleccionar canon Italiano"))
  , Translate (-300) (-50) (Scale 0.4 0.4 (Text "Presiona '3' para seleccionar canon Frances"))
  , Translate (-300) (-100) (Scale 0.4 0.4 (Text "Presiona '4' para seleccionar canon Aleman"))
  , Translate (-300) (-150) (Scale 0.4 0.4 (Text "Presiona '5' para seleccionar canon Chileno"))
  , Translate (-300) (-200) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]

drawCanonSelectionScreen :: Player -> Picture
drawCanonSelectionScreen player = Pictures
  [ Translate (-300) 150 (Scale 0.5 0.5 (Text ("Seleccione su canon - " ++ show player)))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text "Presiona '1' para Canon USA"))
  , Translate (-300) 0  (Scale 0.4 0.4 (Text "Presiona '2' para Canon Italiano"))
  , Translate (-300) (-50) (Scale 0.4 0.4 (Text "Presiona '3' para Canon Frances"))
  , Translate (-300) (-100) (Scale 0.4 0.4 (Text "Presiona '4' para Canon Aleman"))
  , Translate (-300) (-150) (Scale 0.4 0.4 (Text "Presiona '5' para Canon Chileno"))
  , Translate (-300) (-200) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]

drawSelectionSummary :: CannonType -> CannonType -> Picture
drawSelectionSummary p1Cannon p2Cannon = Pictures
  [ Translate (-300) 100 (Scale 0.5 0.5 (Text "Resumen de Seleccion"))
  , Translate (-300) 50 (Scale 0.4 0.4 (Text $ "Jugador 1 selecciono: " ++ show p1Cannon))
  , Translate (-300) 0 (Scale 0.4 0.4 (Text $ "Jugador 2 selecciono: " ++ show p2Cannon))
  , Translate (-300) (-50) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  ]
