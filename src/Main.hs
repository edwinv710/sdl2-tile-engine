{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified TileEngine
import qualified SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

createImage :: String -> SDL.Renderer -> IO TileEngine.Image
createImage path renderer = do 
  surface <- SDL.loadBMP path
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return $ TileEngine.Image path texture size 


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = False
        }

  timage     <- createImage "src/tilesheet.bmp" renderer
  let tileSet = TileEngine.tileset timage (32, 32)
  let tileMap = TileEngine.genTileMap tileSet (20, 15) [ x + y | x <- [0..14], y <- [1..20]]

  drawMap renderer tileMap

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


drawMap renderer tileMap = do
  events <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  TileEngine.renderMap renderer tileMap (0,0) 

  SDL.present renderer
  unless quit (drawMap renderer tileMap)

  {-
data Tileset = Tileset { tilesetImage :: Image, tileSize :: MyPoint, tileClips :: [[SDL.Rectangle]] }
data TimeMap = TileMap { mapArr :: [[Int]], mapTileset :: Tileset }


data SpriteSheet = SpriteSheet { sheetImage     :: Image
                               , sheetDimension :: Dimension } 

data Sprite = Sprite { spriteImage      :: Image
                     , spriteUpperBound :: MyPoint
                     , spriteLowerBound :: MyPoint
                     , spriteDimension  :: Dimension 
                     , spriteUDLR    :: (Int, Int, Int, Int)}

getSheetClips (Image _ _ (V2 width height)) (clipWidth, clipHeight) (xOffset yOffset) =
  [ [ clip (clipWidth, clipHeight) (xOffset, yOffset) (x, y) | x <- [0..(horizontal - 1)] ] | y <- [0..(vertical - 1)]]  where
    horizontal = width `div` clipWidth
    vertical   = height `div` clipHeight
    clip (w, h) (xOff, yOff) (xPos, yPos) = SDL.Rectangle (P (V2 (w * xPos + xOff) (h * yPos + yOff))) (V2 w h)

getMapClips (width, height) (tileWidth, tileHeight) (xOffSet, yOffset) = 
  [[SDL.Rectangle (P (V2 (x * tileWidth + xOffset) (y * tileHeight + yOffset))) (V2 width height) |
    x <- [0..(width - 1)]] | y <- [0..(height -1)]]

getFrame (Sprite _ (upperX, upperY) _ (width, height) _) (x, y) = 
  Just $ SDL.Rectangle (P (V2 (width * x + upperX) (height * y + upperY))) (V2 width height)

getDestination (Sprite _ _ _ (width, height) _) (x, y) = 
  Just $ SDL.Rectangle (P (V2 x y)) (V2 width height)

renderSprite :: Sprite -> MyPoint -> MyPoint -> SDL.Renderer -> IO ()
renderSprite sprite (frameX, frameY) (x, y) renderer = do
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer
  let texture   = (imageTexture . spriteImage) sprite
  let clip        = getFrame sprite (frameX, frameY)
  let  destination = getDestination sprite (x, y)
  SDL.copy renderer texture clip destination

animate sprite state renderer = do

  
  let difference = endTick - startTick
  putStrLn $ show frameTime
  putStrLn $ show difference 
  putStrLn $ show (frameTime - difference)
  SDL.delay $ frameTime - differencer

  renderSprite sprite (frame, 0) (0, 0) renderer



renderTile (Image _ texture _) clip destination renderer = do
  SDL.copy renderer texture clip destination

renderTileRow (x:xs) (Tileset image (width, height) (tileWidth, tileHeight) tileClips) (xIndex, yIndex) = do
  let destination = Just $ SDL.Rectangle (P (V2 (xIndex * width) (yIndex * height))) (V2 width height)
  let (x, y) = tileNumToIndex x image (width, height)
  renderTile image 

tileNumToIndex num (Image _ _ (width, height)) (tileWidth, tileHeight) (columns, _) = do
  ( x , y) where
    x = num `mod` columns
    y = num `div` columns 

tilesetDimension (Timeset (Image _ _ (imgWidth, imgHeight)) (tileWidth, tileHeight)
 
data Tileset = Tileset { tilesetImage :: Image, tileSize :: MyPoint, tileClips :: [[SDL.Rectangle]] }

data Image = Image { imagePath    :: String
                   , imageTexture :: SDL.Texture 
                   , imageSize    :: (V2 CInt) }

sequence_ :: [IO a] -> IO ()
sequence_ [] = return ()
sequence_ (x:xs) = do x
                      sequence_ xs

renderMap :: TileMap -> Tileset -> IO ()
renderMap (TileMap tileMap) (Tileset image (width, height) (tileWidth, tileHeight) tileClips) = do
  let mapClips = getMapClips (20, 15) (32, 32) (0,0)
  let tileMap

   





-}
