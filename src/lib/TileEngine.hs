module TileEngine where

import GHC.Int
import System.IO
import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import SDL.Time
import SDL (($=))
import GameEngine
import Data.List
import Data.List.Split (splitOn)
import qualified SDL

data Tile    = SurfaceTile { 
                             tilesetClip :: Maybe (SDL.Rectangle CInt),
                             mapClip     :: Maybe (SDL.Rectangle CInt),
                             tileCoord   :: Coord }

data Tileset = Tileset     { 
                             tilesetImage :: Image, 
                             tileSize     :: Dimension, 
                             tileClips    :: [Maybe (SDL.Rectangle CInt)] }

data Layer   = Layer       {
                             layerTileset  :: Tileset,
                             layerDimesion :: Dimension,
                             layerTiles    :: [Tile]}

data TileMap = TileMap     { layers :: [Layer] }

{- Tile -}

renderTile :: SDL.Renderer -> SDL.Texture -> Coord -> Tile -> IO ()
renderTile _ _ _ (SurfaceTile Nothing _ _) = do return ()
renderTile renderer texture offset (SurfaceTile tilesetClip mapClip _) = do
  let offsetTile =  offsetTileClip offset mapClip
  SDL.copy renderer texture tilesetClip offsetTile

offsetTileClip :: Coord -> Maybe (SDL.Rectangle CInt) ->  Maybe (SDL.Rectangle CInt) 
offsetTileClip (xOff, yOff) (Just (SDL.Rectangle (P (V2 x y) ) (V2 w h))) =
  _sdlRect ( (x + xOff), (y + yOff) ) (w, h)

generateLayer :: Tileset -> Dimension -> [Char] -> IO Layer
generateLayer set@(Tileset img tsize c) msize path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let tiles = map (\x -> read x :: Int) $ (splitOn ",") . (intercalate ",") $ lines contents
  return $ layer set msize tiles
  
renderLayer :: SDL.Renderer -> Coord -> Layer -> IO ()
renderLayer renderer offset (Layer (Tileset image tSize _) _ tiles) = do
  let texture = imageTexture image
  mapM_ (renderTile renderer texture offset) tiles  
  
renderMap :: SDL.Renderer -> TileMap -> Coord -> IO ()
renderMap renderer (TileMap layers) offset = do
  mapM_ (renderLayer renderer offset) layers  
  
layer :: Tileset -> Dimension -> [Int] -> Layer
layer set@(Tileset img tsize c) msize xs = 
  Layer set msize $ generateMapTiles set msize xs

generateMapTiles :: Tileset -> Dimension -> [Int] -> [Tile]
generateMapTiles set@(Tileset _ tsize@(twidth, theight) tclips) (width, _) nums =
  map tiles indexedNums where
    indexedNums = (zip [0..] nums) 
    tiles (i, n) = SurfaceTile  (clipFromTileset n set) (_clipFromCoord tsize (0,0) coord) coord where
      coord = coordFromNum i width 

clipFromTileset :: Int -> Tileset -> Maybe (SDL.Rectangle CInt)
clipFromTileset n tset | n >= 0    = (tileClips tset) !! n
                       | otherwise = Nothing

coordFromNum :: CInt -> CInt -> Coord 
coordFromNum n w = ( n `mod` w, n `div` w)

tileset :: Image  -> Dimension -> Tileset
tileset img tileDimension = 
  Tileset img tileDimension $ _tilesetClips img tileDimension (0, 0)

_tilesetClips :: Image -> Dimension -> Coord -> [Maybe (SDL.Rectangle CInt)]
_tilesetClips (Image _ _ (V2 width height)) (tileWidth, tileHeight) offset =
  [ _clipFromCoord (tileWidth, tileHeight) offset (x, y) | y <- [0..yMax],  x <- [0..xMax]]  where
    xMax = width `div` tileWidth - 1
    yMax = height `div` tileHeight - 1

_clipFromCoord :: Dimension -> Coord -> Coord -> Maybe (SDL.Rectangle CInt)
_clipFromCoord (twidth, theight) (xOff, yOff) (x, y) =
  _sdlRect ((twidth * x + xOff), (theight * y + yOff)) (twidth, theight) 

_sdlRect :: Coord -> Dimension -> Maybe (SDL.Rectangle CInt)
_sdlRect (x, y) (w, h) =
  Just $ SDL.Rectangle (P (V2 x y)) (V2 w h)
