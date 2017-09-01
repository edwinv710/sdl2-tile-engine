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
(screenWidth, screenHeight) = (933, 698)

createImage :: String -> SDL.Renderer -> IO Image
createImage path renderer = do 
  surface <- SDL.Image.load path
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return $ Image texture size 

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


  timage <-     createImage "src/tiles_spritesheet_12.png" renderer
  let tileset = TileEngine.tileset timage (70, 70) (2,2)

  mainLayer       <- TileEngine.fromCSV tileset (50, 10)  "src/sidescroller_main.csv"
  waterLayer      <- TileEngine.fromCSV tileset (50, 10)  "src/sidescroller_scrolling_water.csv"
  backgroundLayer <- TileEngine.fromCSV tileset (50, 10)  "src/sidescroller_background_objects.csv"
  cloudsLayer     <- TileEngine.fromCSV tileset (50, 10)  "src/sidescroller_scrolling_clouds.csv"

  let tileMap = [mainLayer, waterLayer, backgroundLayer, cloudsLayer]

  drawMap renderer tileMap (0,0) (0,0) (0,0)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

renderMap renderer layers pos = do
  SDL.clear renderer
  SDL.rendererDrawColor renderer $=  V4 199 242 245 maxBound
  SDL.fillRect renderer Nothing
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  mapM_ (TileEngine.renderLayer renderer pos) layers
  SDL.present renderer

pollQuit = do
  events <- SDL.pollEvents
  return (elem SDL.QuitEvent $ map SDL.eventPayload events)

pollMovement = do
  keyMap <- SDL.getKeyboardState
  let offset = 
        if | keyMap SDL.ScancodeDown -> (0, 4)
           | keyMap SDL.ScancodeUp -> (0, -4)
           | keyMap SDL.ScancodeRight -> (4, 0)
           | keyMap SDL.ScancodeLeft -> (-4, 0)
           | otherwise -> (0,0)
  return offset


scrollingOffset (vw, vh) (x, y) layer =
  offsets where
    offsets 
      | rx > (mw - vw) = [(rx, ry), (rx - mw, ry)]
      | otherwise = [(rx, ry)]
    (tw, th) = TileEngine.tileSize $ TileEngine.layerTileset layer
    mw = (*) tw $ fst $ TileEngine.layerDimension layer
    mh = (*) th $ snd $ TileEngine.layerDimension layer
    rx = x `mod` mw
    ry = y `mod` mh


addTuple (vx, vy) (tupx, tupy) = (tupx + vx, tupy + vy)

drawMap renderer layers pos wOffset cOffset = do
  let cloudsLayer     = layers !! 3
  let backgroundLayer = layers !! 2
  let waterLayer      = layers !! 1
  let mainLayer       = layers !! 0

  quit     <- pollQuit 
  distance <- pollMovement

  let mOff = offset pos distance (933, 698) $ mainLayer
  let wOff = addTuple (2, 0) mOff
  let cOff = addTuple (1, 0) mOff

  let cPos = scrollingOffset (933, 698) (addTuple cOffset cOff) cloudsLayer
  let wPos = scrollingOffset (933, 698) (addTuple wOffset wOff) waterLayer
  let mPos = addTuple pos mOff
  
  putStrLn $ show wPos

  SDL.clear renderer
  SDL.rendererDrawColor renderer $=  V4 199 242 245 maxBound
  SDL.fillRect renderer Nothing
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  
  mapM_ (\x -> x cloudsLayer) $ map (TileEngine.renderLayer renderer) cPos
  TileEngine.renderLayer renderer mPos backgroundLayer
  mapM_ (\x -> x waterLayer) $ map (TileEngine.renderLayer renderer) wPos
  TileEngine.renderLayer renderer mPos mainLayer

  SDL.present renderer

  unless quit $ drawMap renderer layers mPos (wPos !! 0) (cPos !! 0)

offset (xPos, yPos) (xOff, yOff) (screenWidth, screenHeight) layer = 
  newOffset (xOff, yOff) (0,0) (value topCorner) $ value bottomCorner where
    topCorner    = ((xPos + xOff), (yPos + yOff))
    bottomCorner = ((xPos + xOff + screenWidth), (yPos + yOff + screenHeight))
    value pos    =  TileEngine.layerValue pos layer

newOffset nOff oOff (Just val) (Just val2) = nOff
newOffset nOff oOff  _ _                   = oOff
