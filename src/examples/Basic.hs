{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified TileEngine
import qualified SDL
import qualified SDL.Image

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (933, 698)

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
  let tileset = TileEngine.tileset (70, 70) (2,2) timage

  layers <- mapM (TileEngine.fromCSV (50, 10) tileset) 
    [ "src/examples/sidescroller_scrolling_clouds.csv"
    , "src/examples/sidescroller_background_objects.csv"
    , "src/examples/sidescroller_scrolling_water.csv"
    , "src/examples/sidescroller_main.csv"]

  drawMap renderer layers (0,0)

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


drawMap renderer layers pos = do
  quit     <- pollQuit 
  
  SDL.clear renderer
  SDL.rendererDrawColor renderer $=  V4 199 242 245 maxBound
  SDL.fillRect renderer Nothing
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
 
  mapM_ (TileEngine.renderLayer renderer pos) layers

  SDL.present renderer

  unless quit $ drawMap renderer layers pos
