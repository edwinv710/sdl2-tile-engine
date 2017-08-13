module GameEngine where

import Foreign.C.Types
import SDL.Vect
import qualified SDL

type Dimension = (CInt, CInt)
type Coord     = (CInt, CInt)

data Image   = Image { imagePath    :: String
                   , imageTexture :: SDL.Texture 
                   , imageSize    :: (V2 CInt) }
