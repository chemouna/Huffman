{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_huffman (
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

bindir     = "/Users/m.cheikhna/Documents/repos/others/freelance/huffman/.stack-work/install/x86_64-osx/lts-8.13/8.0.2/bin"
libdir     = "/Users/m.cheikhna/Documents/repos/others/freelance/huffman/.stack-work/install/x86_64-osx/lts-8.13/8.0.2/lib/x86_64-osx-ghc-8.0.2/huffman-0.1.0.0-JEXluJcztEkJwZY01k8hC7"
dynlibdir  = "/Users/m.cheikhna/Documents/repos/others/freelance/huffman/.stack-work/install/x86_64-osx/lts-8.13/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/m.cheikhna/Documents/repos/others/freelance/huffman/.stack-work/install/x86_64-osx/lts-8.13/8.0.2/share/x86_64-osx-ghc-8.0.2/huffman-0.1.0.0"
libexecdir = "/Users/m.cheikhna/Documents/repos/others/freelance/huffman/.stack-work/install/x86_64-osx/lts-8.13/8.0.2/libexec"
sysconfdir = "/Users/m.cheikhna/Documents/repos/others/freelance/huffman/.stack-work/install/x86_64-osx/lts-8.13/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "huffman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "huffman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "huffman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "huffman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "huffman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "huffman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
