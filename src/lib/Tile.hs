module Tile where

import GameEngine
import Tileset
import Foreign.C.Types
import SDL.Vect
import qualified SDL

data Tile = SurfaceTile { tileImgRect   :: Maybe (SDL.Rectangle CInt),
                          tileRect  :: Maybe (SDL.Rectangle CInt),
                          tileCoord :: Coord,
                          tileValue :: Int } 
          | EventTile   { 
                          tileCoord :: Coord,
                          tileValue :: Int }

-- Renders a tile from a given texture with the offset given.
renderTile :: SDL.Renderer -> Tileset -> Coord -> Tile -> IO ()
renderTile renderer tileset offset (SurfaceTile tilesetClip@(Just _) mapClip _ _) = do
  let offsetTile =  offsetClip offset mapClip
  SDL.copy renderer (tilesetTexture tileset) tilesetClip offsetTile
renderTile _ _ _ _ = do return ()

offsetClip :: Coord -> Maybe (SDL.Rectangle CInt) ->  Maybe (SDL.Rectangle CInt) 
offsetClip (xOff, yOff) (Just (SDL.Rectangle (P (V2 x y) ) (V2 w h))) =
  sdlRect ( (x - xOff), (y - yOff) ) (w, h)
