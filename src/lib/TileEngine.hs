module TileEngine where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL

type Dimension   = (CInt, CInt)
type Coord     = (CInt, CInt)

data Image = Image { imagePath    :: String
                   , imageTexture :: SDL.Texture 
                   , imageSize    :: (V2 CInt) }

data Tileset = Tileset { 
                         tilesetImage :: Image, 
                         tileSize     :: Dimension, 
                         tileClips    :: [Maybe (SDL.Rectangle CInt)] }

data TileMap = TileMap { 
                         mapTileset :: Tileset ,
                         mapSize    :: Dimension,
                         mapLst     :: [(Maybe (SDL.Rectangle CInt), Coord)]} 

renderTiles :: SDL.Renderer -> SDL.Texture -> Coord -> Dimension -> [(Maybe (SDL.Rectangle CInt), Coord)] -> IO ()
renderTiles _ _  _ _ [] = do return ()
renderTiles renderer texture (xOff, yOff) (tWidth, tHeight) (x:xs) = do 
  let tileRec          = fst x
  let (xCoord, yCoord) = snd x
  let x = xCoord * tWidth + xOff
  let y = yCoord * tHeight + yOff
  let rec = _sdlRect (x, y) (tWidth, tHeight)
  SDL.copy renderer texture tileRec rec
  renderTiles renderer texture (xOff, yOff) (tWidth, tHeight) xs

renderMap :: SDL.Renderer -> TileMap -> Coord -> IO ()
renderMap renderer (TileMap (Tileset image tSize@(tWidth, tHeight) _) (width, height) tiles) off = do
  let texture        = imageTexture image
  renderTiles renderer texture off tSize tiles
  
genTileMap :: Tileset -> Dimension -> [Int] -> TileMap
genTileMap set@(Tileset img (tW, tH) c) s@(w, h) xs = TileMap set s $  map f (zip [0..] xs) where
  f x = ( ( clipFromTileset (snd x) set ), ( coordFromNum (fst x) w      ))


clipFromTileset :: Int -> Tileset -> Maybe (SDL.Rectangle CInt)
clipFromTileset n tset = (tileClips tset) !! n

coordFromNum n w = ( n `mod` w, n `div` w)

tileset :: Image  -> Dimension -> Tileset
tileset img tileDim = Tileset img tileDim $ _tilesetClips img tileDim (0, 0)

_tilesetClips :: Image -> Dimension -> Coord -> [Maybe (SDL.Rectangle CInt)]
_tilesetClips (Image _ _ (V2 width height)) (tileWidth, tileHeight) (xOffset, yOffset) =
  [ clip (tileWidth, tileHeight) (xOffset, yOffset) (x, y) | x <- [0..xMax],  y <- [0..yMax]]  where
    xMax = width `div` tileWidth - 1
    yMax = height `div` tileHeight - 1
    clip (w, h) (xOff, yOff) (x, y) = _sdlRect ((w * x + xOff), (h * y + yOff)) (w, h) 
    
_sdlRect :: Coord -> Dimension -> Maybe (SDL.Rectangle CInt)
_sdlRect (x, y) (w, h) =
  Just $ SDL.Rectangle (P (V2 x y)) (V2 w h)
