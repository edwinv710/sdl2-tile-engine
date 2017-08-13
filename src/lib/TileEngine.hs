module TileEngine where

import GHC.Int
import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import SDL.Time
import SDL (($=))
import GameEngine
import qualified SDL

data Tile    = SurfaceTile { 
                             tilesetClip :: Maybe (SDL.Rectangle CInt),
                             mapClip     :: Maybe (SDL.Rectangle CInt),
                             tileCoord   :: Coord
                           }

data Tileset = Tileset { 
                         tilesetImage :: Image, 
                         tileSize     :: Dimension, 
                         tileClips    :: [Maybe (SDL.Rectangle CInt)] }

data TileMap = TileMap { 
                         mapTileset :: Tileset ,
                         mapSize    :: Dimension,
                         mapLst     :: [Tile]} 


renderTile :: SDL.Renderer -> SDL.Texture -> Coord -> Tile -> IO ()
renderTile renderer texture offset (SurfaceTile tilesetClip mapClip _) = do
  let offsetTile =  offsetTileClip offset mapClip
  SDL.copy renderer texture tilesetClip offsetTile

offsetTileClip :: Coord -> Maybe (SDL.Rectangle CInt) ->  Maybe (SDL.Rectangle CInt) 
offsetTileClip (xOff, yOff) (Just (SDL.Rectangle (P (V2 x y) ) (V2 w h))) =
  _sdlRect ( (x + xOff), (y + yOff) ) (w, h)

renderTiles :: SDL.Renderer -> SDL.Texture -> Coord -> [Tile] -> IO ()
renderTiles _ _ _ [] = do return ()
renderTiles renderer texture offset (x:xs) = do 
  renderTile renderer texture offset x  
  renderTiles renderer texture offset xs  

renderMap :: SDL.Renderer -> TileMap -> Coord -> IO ()
renderMap renderer (TileMap (Tileset image tSize _) _ tiles) offset = do
  let texture = imageTexture image
  renderTiles renderer texture offset tiles
  
tileMap :: Tileset -> Dimension -> [Int] -> TileMap
tileMap set@(Tileset img tsize c) msize xs = 
  TileMap set msize $ generateMapTiles set tsize xs

generateMapTiles :: Tileset -> Dimension -> [Int] -> [Tile]
generateMapTiles set@(Tileset _ tsize@(twidth, theight) tclips) (width, _) nums =
  map tiles indexedNums where
    indexedNums = (zip [0..] nums) 
    tiles (i, n) = SurfaceTile  (clipFromTileset n set) (_clipFromCoord tsize (0,0) coord) coord where
      coord = coordFromNum i 20

clipFromTileset :: Int -> Tileset -> Maybe (SDL.Rectangle CInt)
clipFromTileset n tset = (tileClips tset) !! n

coordFromNum :: CInt -> CInt -> Coord 
coordFromNum n w = ( n `mod` w, n `div` w)

tileset :: Image  -> Dimension -> Tileset
tileset img tileDimension = 
  Tileset img tileDimension $ _tilesetClips img tileDimension (0, 0)

_tilesetClips :: Image -> Dimension -> Coord -> [Maybe (SDL.Rectangle CInt)]
_tilesetClips (Image _ _ (V2 width height)) (tileWidth, tileHeight) offset =
  [ _clipFromCoord (tileWidth, tileHeight) offset (x, y) | x <- [0..xMax],  y <- [0..yMax]]  where
    xMax = width `div` tileWidth - 1
    yMax = height `div` tileHeight - 1

_clipFromCoord :: Dimension -> Coord -> Coord -> Maybe (SDL.Rectangle CInt)
_clipFromCoord (twidth, theight) (xOff, yOff) (x, y) =
  _sdlRect ((twidth * x + xOff), (theight * y + yOff)) (twidth, theight) 

_sdlRect :: Coord -> Dimension -> Maybe (SDL.Rectangle CInt)
_sdlRect (x, y) (w, h) =
  Just $ SDL.Rectangle (P (V2 x y)) (V2 w h)
