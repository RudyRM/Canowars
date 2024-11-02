module NextScreen (drawNextScreen) where

import Graphics.Gloss

drawNextScreen :: Picture
drawNextScreen = Pictures
  [ Translate (-150) 0 (Scale 0.5 0.5 (Text "¡Bienvenido a la siguiente pantalla!"))
  , Translate (-150) (-50) (Scale 0.3 0.3 (Text "Presiona 'q' para volver al menu"))
  , Translate (-150) (-100) (Scale 0.3 0.3 (Text "Presiona 'Espacio' para accion especial"))
  ]
