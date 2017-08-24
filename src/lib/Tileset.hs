module Tileset  where

import GameEngine
import Foreign.C.Types
import SDL.Vect
import qualified SDL

data Tileset = Tileset { tilesetImage :: Image, 
                         tileSize     :: Dimension, 
                         tileClips    :: [Maybe (SDL.Rectangle CInt)] }

tileset :: Image  -> Dimension -> Tileset
tileset img tileDimension = 
  Tileset img tileDimension $ clips img tileDimension (0, 0)

clips :: Image -> Dimension -> Coord -> [Maybe (SDL.Rectangle CInt)]
clips (Image _ _ (V2 width height)) (tileWidth, tileHeight) offset =
  [ sdlRectCoord (tileWidth, tileHeight) offset (x, y) | y <- [0..yMax],  x <- [0..xMax]]  where
    xMax = width `div` tileWidth - 1
    yMax = height `div` tileHeight - 1

clip :: Int -> Tileset -> Maybe (SDL.Rectangle CInt)
clip n tset | n >= 0    = (tileClips tset) !! n
                       | otherwise = Nothing

tilesetTexture :: Tileset -> SDL.Texture
tilesetTexture tileset = 
  imageTexture $ tilesetImage tileset
