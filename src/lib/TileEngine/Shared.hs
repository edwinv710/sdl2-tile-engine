module TileEngine.Shared where

import Foreign.C.Types
import SDL.Vect
import qualified SDL

type Dimension = (CInt, CInt)
type Coord     = (CInt, CInt)

data Image = Image { imageTexture :: SDL.Texture 
                   , imageSize    :: (V2 CInt) } -- ^ The Image used for the tileset. Contains both the image texture and the dimension of the image texture.
