module TileEngine.Tileset  where

import TileEngine.Shared
import TileEngine.Internal
import Foreign.C.Types
import SDL.Vect
import qualified SDL

data Tileset = Tileset { tilesetImage   :: Image
                         -- ^ Returns the Image
                       , tileSize       :: Dimension
                         -- ^ The individual tile size. eg: (32, 32)
                       , tilesetSpacing :: Dimension
                         -- ^ The spacing between each tile in the tileset
                       , tileClips      :: [Maybe (SDL.Rectangle CInt)] 
                         -- ^ All the Rectangles relative to the Tileset
                       } 

-- | Returns a Tileset. Accepts the accociated Image, tile dimension, and spacing between Tiles.
tileset :: Dimension -> Dimension -> Image  -> Tileset
tileset dimension spacing img = 
  Tileset img dimension spacing $ clips img dimension spacing (0, 0)

clip :: Int -> Tileset -> Maybe (SDL.Rectangle CInt)
clip n tset | n >= 0    = (tileClips tset) !! n
                       | otherwise = Nothing
                     
