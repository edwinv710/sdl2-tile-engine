module GameEngine where

import Foreign.C.Types
import SDL.Vect
import qualified SDL

type Dimension = (CInt, CInt)
type Coord     = (CInt, CInt)

data Image = Image { imagePath    :: String
                   , imageTexture :: SDL.Texture 
                   , imageSize    :: (V2 CInt) }

sdlRect :: Coord -> Dimension -> Maybe (SDL.Rectangle CInt)
sdlRect (x, y) (w, h) =
  Just $ SDL.Rectangle (P (V2 x y)) (V2 w h)

sdlRectCoord :: Dimension -> Coord -> Coord -> Maybe (SDL.Rectangle CInt)
sdlRectCoord (twidth, theight) (xOff, yOff) (x, y) =
  sdlRect ((twidth * x + xOff), (theight * y + yOff)) (twidth, theight) 

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
