{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL
import SDL (($=))
import GameEngine
import qualified TileEngine
import qualified SDL
import qualified SDL.Image

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (933, 698)

data ScrollableLayer = SLayer TileEngine.Layer (CInt, CInt) (CInt, CInt)

createImage :: String -> SDL.Renderer -> IO TileEngine.Image
createImage path renderer = do 
  surface <- SDL.Image.load path
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return $ TileEngine.Image texture size 

pollQuit = do
  events <- SDL.pollEvents
  return (elem SDL.QuitEvent $ map SDL.eventPayload events)

pollMovement speed = do
  keyMap <- SDL.getKeyboardState
  let negatedSpeed = negate speed
  let offset = 
        if | keyMap SDL.ScancodeDown  -> (0, speed)
           | keyMap SDL.ScancodeRight -> (speed, 0)
           | keyMap SDL.ScancodeUp    -> (0, negatedSpeed)
           | keyMap SDL.ScancodeLeft  -> (negatedSpeed, 0)
           | otherwise                -> (0,0)
  return offset

addTuple (vx, vy) (tupx, tupy) = (tupx + vx, tupy + vy)

newCoordinate layer (sw, sh) (ox, oy) (x, y) =
  maybe (ox, oy) (\_ -> (x, y)) $ topCorner >> bottomCorner where
    topCorner    = TileEngine.layerPValue (x, y) layer
    bottomCorner = TileEngine.layerPValue (x + sw, y + sh) layer

renderLayer :: SDL.Renderer -> (CInt, CInt) -> ScrollableLayer -> IO ScrollableLayer 
renderLayer renderer off (SLayer layer pos speed) = do
  let npos = addTuple pos $ addTuple speed off 
  TileEngine.renderLayer renderer npos layer
  return $ SLayer layer npos speed

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
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  timage <-     createImage "src/examples/tiles_spritesheet_12.png" renderer
  let tileset = TileEngine.tileset timage (70, 70) (2,2)

  mainLayer       <- TileEngine.fromCSV tileset (50, 10)  "src/examples/sidescroller_main.csv"
  waterLayer      <- TileEngine.fromCSV tileset (50, 10)  "src/examples/sidescroller_scrolling_water.csv"
  cloudsLayer     <- TileEngine.fromCSV tileset (50, 10)  "src/examples/sidescroller_scrolling_clouds.csv"
  backgroundLayer <- TileEngine.fromCSV tileset (50, 10)  "src/examples/sidescroller_background_objects.csv"

  let layers = [ SLayer cloudsLayer     (0,0) (1,0), SLayer backgroundLayer (0,0) (0,0), SLayer waterLayer      (0,0) (2,0), SLayer mainLayer       (0,0) (0,0)]

  drawMap renderer layers (0,0)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


drawMap renderer layers pos = do
  quit     <- pollQuit 
  movement <- pollMovement 4  

  SDL.clear renderer
  SDL.rendererDrawColor renderer $=  V4 199 242 245 maxBound
  SDL.fillRect renderer Nothing
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
 
  newLayers <- mapM (renderLayer renderer (0,0)) layers
  {-mapM_ (TileEngine.renderLayer renderer newCoord) layers-}

  SDL.present renderer

 {-  unless quit $ drawMap renderer layers newCoord -}
  unless quit $ drawMap renderer newLayers (0,0) 
