module TileEngine.Layer where

import TileEngine.Shared
import TileEngine.Internal
import TileEngine.Tileset
import TileEngine.Tile
import System.IO
import Data.List
import Data.List.Split (splitOn)
import qualified SDL

data Layer = TileLayer  { layerTileset   :: Tileset
                          -- ^ Return the Tileset
                        , layerDimension :: Dimension
                          -- ^ The layer dimension realtive to the number of tiles. eg: (20, 15)
                        , layerTiles     :: [Tile]
                          -- ^ A list of the the Tiles in the Layer.
                          } -- ^ A visible Layer.
           | EventLayer { layerDimension     :: Dimension
                          -- ^ The layer dimension realtive to the number of tiles. eg: (20, 15)
                        , layerTileDimension :: Dimension
                          -- ^ The dimension of each tile. eg: (32, 32)
                        , layerTiles         :: [Tile] 
                          -- ^ A list of the the Tiles in the Layer.
                          } -- ^ A non-visibile layer. Can be used to trigger event such as tile collision.

-- | Renders a layer at a given coordinate using the renderer passed.
renderLayer :: SDL.Renderer -> Coord -> Layer -> IO ()
renderLayer renderer offset (TileLayer tileset _ tiles) = do
  mapM_ (renderTile renderer tileset offset) tiles  

-- | Creates a layer from the given path to the csv file.
fromCSV :: Dimension -> Tileset -> [Char] -> IO Layer
fromCSV msize set path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let tiles = map (\x -> read x :: Int) $ (splitOn ",") . (intercalate ",") $ lines contents
  return $ layer msize set tiles

-- | Returns a Layer using the EventLayer constructor. Event Layers do not have tilesets associated with them. The tile value can be used to perform an action.
eventLayer :: Dimension -> Dimension -> [Int] -> Layer
eventLayer msize tsize xs =
  EventLayer msize tsize $ generateEventTiles tsize msize xs

-- | Returns a Layer using the TileLayer constructor. 
layer :: Dimension -> Tileset -> [Int] -> Layer
layer msize set xs = 
  TileLayer set msize $ generateTiles set msize xs

generateTiles :: Tileset -> Dimension -> [Int] -> [Tile]
generateTiles set@(Tileset _ tsize spacing _) (width, _) nums =
  map tiles indexedNums where
    indexedNums = (zip [0..] nums) 
    tiles (i, n) = SurfaceTile  (clip n set) (sdlRectCoord tsize (0,0) (0,0) coord) n where
      coord = coordFromNum i width 

generateEventTiles :: Dimension -> Dimension -> [Int] -> [Tile]
generateEventTiles size@(width, height) tsize xs = 
  map tiles indexedNums where
   indexedNums = (zip [0..] xs) 
   tiles (i, n) = EventTile n where

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
