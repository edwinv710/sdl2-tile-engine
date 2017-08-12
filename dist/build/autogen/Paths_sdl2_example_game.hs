{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_sdl2_example_game (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/edwinvelasquez/Library/Haskell/bin"
libdir     = "/Users/edwinvelasquez/Library/Haskell/ghc-8.0.2-x86_64/lib/sdl2-example-game-0.1.0.0"
dynlibdir  = "/Users/edwinvelasquez/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/edwinvelasquez/Library/Haskell/share/ghc-8.0.2-x86_64/sdl2-example-game-0.1.0.0"
libexecdir = "/Users/edwinvelasquez/Library/Haskell/libexec"
sysconfdir = "/Users/edwinvelasquez/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sdl2_example_game_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sdl2_example_game_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sdl2_example_game_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sdl2_example_game_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sdl2_example_game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sdl2_example_game_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
