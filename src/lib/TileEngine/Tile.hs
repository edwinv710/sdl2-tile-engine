module TileEngine.Tile where

import TileEngine.Shared
import TileEngine.Internal
import TileEngine.Tileset
import Foreign.C.Types
import SDL.Vect
import qualified SDL

data Tile = SurfaceTile { tileImgRect :: Maybe (SDL.Rectangle CInt)
                          -- ^ Rectangle corresponding to the section of the tileset texture the tile comes from.
                        , tileRect    :: Maybe (SDL.Rectangle CInt)
                          -- ^ Rectangle corresponding to the section of the screen the tile is to be placed.
                        , tileValue   :: Int 
                          -- The numerical value of the tile.
                        } 
                          -- ^ Tile corresponding to a tileset.
          | EventTile   { tileValue   :: Int 
                          -- ^ The numerical value of the tile.
                        }  
                          -- ^ Tile that is not visible. Can be used for collision detection etc.

-- | Renders a tile at a given coordinate using a given tileset.
renderTile :: SDL.Renderer -> Tileset -> Coord -> Tile -> IO ()
renderTile renderer tileset offset (SurfaceTile tilesetClip@(Just _) mapClip _) = do
  let offsetTile =  offsetClip offset mapClip
  SDL.copy renderer (tilesetTexture tileset) tilesetClip offsetTile
renderTile _ _ _ _ = do return ()


tilesetTexture = imageTexture . tilesetImage
