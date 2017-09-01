module Layer where

import Tileset
import Tile
import System.IO
import GameEngine
import Data.List
import Data.List.Split (splitOn)
import qualified SDL

data Layer = TileLayer          { layerTileset       :: Tileset,
                                  layerDimension      :: Dimension,
                                  layerTiles         :: [Tile]}
           | EventLayer         { layerDimension     :: Dimension,
                                  layerTileDimension :: Dimension,
                                  layerTiles         :: [Tile] }

renderLayer :: SDL.Renderer -> Coord -> Layer -> IO ()
renderLayer renderer offset (TileLayer tileset _ tiles) = do
  mapM_ (renderTile renderer tileset offset) tiles  

fromCSV :: Tileset -> Dimension -> [Char] -> IO Layer
fromCSV set msize path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let tiles = map (\x -> read x :: Int) $ (splitOn ",") . (intercalate ",") $ lines contents
  return $ layer set msize tiles

eventLayer :: Dimension -> Dimension -> [Int] -> Layer
eventLayer msize tsize xs =
  EventLayer msize tsize $ generateEventTiles tsize msize xs

layer :: Tileset -> Dimension -> [Int] -> Layer
layer set msize xs = 
  TileLayer set msize $ generateTiles set msize xs

generateTiles :: Tileset -> Dimension -> [Int] -> [Tile]
generateTiles set@(Tileset _ tsize spacing _) (width, _) nums =
  map tiles indexedNums where
    indexedNums = (zip [0..] nums) 
    tiles (i, n) = SurfaceTile  (Tileset.clip n set) (sdlRectCoord tsize (0,0) (0,0) coord) coord n where
      coord = coordFromNum i width 

generateEventTiles :: Dimension -> Dimension -> [Int] -> [Tile]
generateEventTiles size@(width, height) tsize xs = 
  map tiles indexedNums where
   indexedNums = (zip [0..] xs) 
   tiles (i, n) = EventTile (coordFromNum i width) n where

layerValue :: Coord -> Layer -> Maybe Int
layerValue coord (EventLayer size tsize tiles) = 
  _tileValue coord size (1,1) tiles 
layerValue coord (TileLayer (Tileset _ tsize _ _ ) size tiles)  = 
  _tileValue coord size (1,1) tiles 

layerPValue :: Coord -> Layer -> Maybe Int
layerPValue coord (EventLayer size tsize tiles) = 
  _tileValue coord size tsize tiles 
layerPValue coord (TileLayer (Tileset _ tsize _ _ ) size tiles)  = 
  _tileValue coord size tsize tiles 

_tileValue :: Coord -> Dimension -> Dimension -> [Tile] -> Maybe Int
_tileValue coord dimension tsize@(twidth, theight) tiles | 
  not $  boxContains (0,0) (pixelDimension dimension tsize) coord (1,1) = Nothing
  | otherwise =  Just $  tileValue tile where
    _coord = coordinate coord tsize
    index  = numFromCoord twidth _coord
    tile   = tiles !! index
