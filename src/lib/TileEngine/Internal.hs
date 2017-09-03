module TileEngine.Internal where

import TileEngine.Shared

import TileEngine.Shared
import Foreign.C.Types
import SDL.Vect
import qualified SDL

{-- Tile --}
offsetClip :: Coord -> Maybe (SDL.Rectangle CInt) ->  Maybe (SDL.Rectangle CInt) 
offsetClip (xOff, yOff) (Just (SDL.Rectangle (P (V2 x y) ) (V2 w h))) =
  sdlRect ( (x - xOff), (y - yOff) ) (w, h)

{-- Tileset --}
clips :: Image -> Dimension -> Dimension -> Coord -> [Maybe (SDL.Rectangle CInt)]
clips (Image _ (V2 width height)) (tileWidth, tileHeight) spacing@(xSpacing, ySpacing) offset =
  [ sdlRectCoord (tileWidth, tileHeight) offset spacing (x, y) | y <- [0..(yMax - 1)],  x <- [0..(xMax - 1)]]  where
    xMax = CInt . round . foldr (/) 1 $ map fromIntegral [width,  tileWidth  + xSpacing]
    yMax = CInt . round . foldr (/) 1 $ map fromIntegral [height, tileHeight + ySpacing]

{-- Shared --}

sdlRect :: Coord -> Dimension -> Maybe (SDL.Rectangle CInt)
sdlRect (x, y) (w, h) =
  Just $ SDL.Rectangle (P (V2 x y)) (V2 w h)

sdlRectCoord :: Dimension -> Coord -> Dimension -> Coord -> Maybe (SDL.Rectangle CInt)
sdlRectCoord (twidth, theight) (xOff, yOff) (xs, ys) (x, y) =
  sdlRect (( (twidth + xs) * x + xOff), ((theight + ys) * y + yOff)) (twidth, theight) 

coordFromNum :: CInt -> CInt -> Coord 
coordFromNum n w = ( n `mod` w, n `div` w)

numFromCoord :: CInt -> Coord -> Int
numFromCoord (CInt width) ((CInt x), (CInt y)) = fromIntegral (y * width + x) :: Int

boxCollision :: Coord -> Dimension -> Coord -> Dimension -> Bool
boxCollision (x1, y1) (w1, h1) (x2, y2) (w2, h2) = (oneDIntersect (x1, w1) (x2, w2)) && (oneDIntersect (y1, h1) (y2, h2)) where
  oneDIntersect (p1, d1) (p2, d2) = (p1 + d1) >= p2 && p1 <= (p2 + d2)

boxContains :: Coord -> Dimension -> Coord -> Dimension -> Bool
boxContains (x2, y2) (w2, h2) (x1, y1) (w1, h1) = 
  x1 >= x2 && (x1 + w1) < (x2 + w2) && y1 >= y2 && (y1 + h1) < (y2 + h2)

pixelDimension :: Dimension -> Dimension -> Dimension
pixelDimension (width, height) (twidth, theight) =
  (pwidth, pheight) where
    pwidth = width * twidth
    pheight = height * theight

coordinate :: Coord -> Dimension -> Coord
coordinate (x, y) (width, height) =
  (cx, cy) where
    cx = x `div` width
    cy = y `div` height
