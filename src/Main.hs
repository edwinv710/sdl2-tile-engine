{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import GameEngine
import qualified TileEngine
import qualified SDL
import qualified SDL.Image

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

createImage :: String -> SDL.Renderer -> IO Image
createImage path renderer = do 
  surface <- SDL.Image.load path
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return $ Image path texture size 

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Tile Engine" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

  timage <-     createImage "src/rpgtiles.png" renderer
  let tileset = TileEngine.tileset timage (32, 32)
  layer01 <-    TileEngine.fromCSV tileset (40, 30)  "src/rpgmap_ground.csv"
  layer02 <-    TileEngine.fromCSV tileset (40, 30)  "src/rpgmap_object.csv"
  let tileMap = [layer01, layer02]

  drawMap renderer tileMap (0,0)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

drawMap renderer layers oldPos = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer
  
  mapM_ (TileEngine.renderLayer renderer oldPos) layers
  
  keyMap <- SDL.getKeyboardState
  let offset = 
        if | keyMap SDL.ScancodeDown -> (0, 1)
           | keyMap SDL.ScancodeUp -> (0, -1)
           | keyMap SDL.ScancodeRight -> (1, 0)
           | keyMap SDL.ScancodeLeft -> (-1, 0)
           | otherwise -> (0,0)

  let pos = position oldPos offset (640, 480) $ layers !! 0

  SDL.present renderer
 
  unless quit (drawMap renderer layers pos)

position (xPos, yPos) (xOff, yOff) (screenWidth, screenHeight) layer = 
  newPosition topCorner (xPos, yPos) (value topCorner) $ value bottomCorner where
    topCorner    = ((xPos + xOff), (yPos + yOff))
    bottomCorner = ((xPos + xOff + screenWidth), (yPos + yOff + screenHeight))
    value pos    =  TileEngine.layerValue pos layer

newPosition newPos oldPos (Just val) (Just val2) = newPos
newPosition newPos oldPos  _ _                   = oldPos 
