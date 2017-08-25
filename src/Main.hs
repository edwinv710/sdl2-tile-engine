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
(screenWidth, screenHeight) = (934, 700)

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

  timage <-     createImage "src/tiles_spritesheet_12.png" renderer
  let tileset = TileEngine.tileset timage (70, 70) (2,2)
  layer01 <-    TileEngine.fromCSV tileset (50, 10)  "src/sidescroller.csv"
  {- layer02 <-    TileEngine.fromCSV tileset (40, 30)  "src/rpgmap_object.csv" -}
  let tileMap = [layer01]

  drawMap renderer tileMap (0,0)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

renderMap renderer layers pos = do
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer
  mapM_ (TileEngine.renderLayer renderer pos) layers
  SDL.present renderer

pollQuit = do
  events <- SDL.pollEvents
  return (elem SDL.QuitEvent $ map SDL.eventPayload events)

pollMovement = do
  keyMap <- SDL.getKeyboardState
  let offset = 
        if | keyMap SDL.ScancodeDown -> (0, 1)
           | keyMap SDL.ScancodeUp -> (0, -1)
           | keyMap SDL.ScancodeRight -> (1, 0)
           | keyMap SDL.ScancodeLeft -> (-1, 0)
           | otherwise -> (0,0)
  return offset

drawMap renderer layers pos = do
  renderMap renderer layers pos
  quit <- pollQuit 
  offset <- pollMovement
  let newPos = position pos offset (640, 480) $ layers !! 0
  unless quit $ drawMap renderer layers newPos

position (xPos, yPos) (xOff, yOff) (screenWidth, screenHeight) layer = 
  newPosition topCorner (xPos, yPos) (value topCorner) $ value bottomCorner where
    topCorner    = ((xPos + xOff), (yPos + yOff))
    bottomCorner = ((xPos + xOff + screenWidth), (yPos + yOff + screenHeight))
    value pos    =  TileEngine.layerValue pos layer

newPosition newPos oldPos (Just val) (Just val2) = newPos
newPosition newPos oldPos  _ _                   = oldPos 
