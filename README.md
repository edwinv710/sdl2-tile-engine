### SDL2TileEngine

SDL2TileEngine is a multi-layered tile-map engine for the creation of games using Haskell's SDL2 library (https://hackage.haskell.org/package/sdl2). 

#### Setup:

Before jumping into writing code, there are a few pre-requisite. First and most obvious, 
you will need to install SDL2 on your machine. We will not go deep into the installation process here, as there 
are many examples such as Lazy Foo' excellent installation guide (http://lazyfoo.net/tutorials/SDL/01_hello_SDL/index.php).
By default, SDL2 supports BMP files. To use PNG files and take advantage of transparency in tiles, install SDL2 Image extension (http://lazyfoo.net/tutorials/SDL/06_extension_libraries_and_loading_other_image_formats/index.php).

Once SDL2 is on your machine, it is time to install the necessary SDL2 wrappers for Haskell <https://hackage.haskell.org/package/sdl2>. Make sure to also install SDL2 Image wrapper (https://hackage.haskell.org/package/sdl2-image).

### Getting Started:

Import the TileEngine library.

```haskell
import qualified TileEngine
```

The first step in creating a tile-based map is to create an Image for our Tileset. An Image is composed of
an image texture and the dimension of that image texture.  SDL2 provides functions that facilitate the creation of an image texture from a file path and 
obtaining the dimension of that texture.
Below is an example function that completes the process of creating an Image.

```haskell
createImage :: SDL.Renderer -> String -> IO TileEngine.Image
createImage renderer path = do 
  surface <- SDL.Image.load path
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return $ TileEngine.Image texture size 
```

The Image can then be passed to the `TileEngine.tileset` function to create a Tileset.

```haskell
timage <- createImage "img/tilesheet.png"
let tileset = TileEngine.tileset (70, 70) (2, 2) timage
```

#### Layers:

There are two ways to create a layer. The easiest is to use the `TileEngine.fromCSV`
function. The function allows you to import your standard comma-separated map layers exported from popular
tile map editors like Tiled. 

```haskell
layer <- TileEngine.fromCSV (20, 15) tileset "map/layer01.csv"
```

If you would like to pre-process your map layers, the `TileEngine.layer` method
allows you create a layer by manually passing a list of Ints.

```haskell
let layer = TileEngine.layer (20, 15) tileset vals
```

To render a layer, use the `TileEngine.renderLayer` method. In addition to the layer that will be rendered, a 
coordinate is passed corresponding to the top-left offset of the layer. Change this value to scroll the layer.

```haskell
renderLayer renderer (0,0) layer
```

#### Dive Deeper:

Examples can be found by visiting the project's github page (https://github.com/edwinv710/sdl2-tile-engine/tree/master/src/examples)
