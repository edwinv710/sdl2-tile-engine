module Tileset  where

import GameEngine
import Foreign.C.Types
import SDL.Vect
import qualified SDL

data Tileset = Tileset { tilesetImage :: Image, 
                         tileSize     :: Dimension, 
                         tilesetSpacing :: Dimension,
                         tileClips    :: [Maybe (SDL.Rectangle CInt)] }

tileset :: Image  -> Dimension -> Dimension -> Tileset
tileset img dimension spacing = 
  Tileset img dimension spacing $ clips img dimension spacing (0, 0)

clips :: Image -> Dimension -> Dimension -> Coord -> [Maybe (SDL.Rectangle CInt)]
clips (Image _ _ (V2 width height)) (tileWidth, tileHeight) spacing@(xSpacing, ySpacing) offset =
  [ sdlRectCoord (tileWidth, tileHeight) offset spacing (x, y) | y <- [0..(yMax - 1)],  x <- [0..(xMax - 1)]]  where
    xMax = CInt . round . foldr (/) 1 $ map fromIntegral [width,  tileWidth  + xSpacing]
    yMax = CInt . round . foldr (/) 1 $ map fromIntegral [height, tileHeight + ySpacing]

clip :: Int -> Tileset -> Maybe (SDL.Rectangle CInt)
clip n tset | n >= 0    = (tileClips tset) !! n
                       | otherwise = Nothing

tilesetTexture :: Tileset -> SDL.Texture
tilesetTexture tileset = 
  imageTexture $ tilesetImage tileset
