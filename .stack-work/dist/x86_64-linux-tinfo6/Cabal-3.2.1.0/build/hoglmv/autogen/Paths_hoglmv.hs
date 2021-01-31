{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hoglmv (
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

bindir     = "/home/dave/Documents/CS/haskell/hoglmv/.stack-work/install/x86_64-linux-tinfo6/a6d4fba4f9f76e5f2af2746d1d44995cf3678c60215993938dd0cec1bf7a477e/8.10.3/bin"
libdir     = "/home/dave/Documents/CS/haskell/hoglmv/.stack-work/install/x86_64-linux-tinfo6/a6d4fba4f9f76e5f2af2746d1d44995cf3678c60215993938dd0cec1bf7a477e/8.10.3/lib/x86_64-linux-ghc-8.10.3/hoglmv-0.1.0.0-37hboWicPYCKbpp9KRugb9-hoglmv"
dynlibdir  = "/home/dave/Documents/CS/haskell/hoglmv/.stack-work/install/x86_64-linux-tinfo6/a6d4fba4f9f76e5f2af2746d1d44995cf3678c60215993938dd0cec1bf7a477e/8.10.3/lib/x86_64-linux-ghc-8.10.3"
datadir    = "/home/dave/Documents/CS/haskell/hoglmv/.stack-work/install/x86_64-linux-tinfo6/a6d4fba4f9f76e5f2af2746d1d44995cf3678c60215993938dd0cec1bf7a477e/8.10.3/share/x86_64-linux-ghc-8.10.3/hoglmv-0.1.0.0"
libexecdir = "/home/dave/Documents/CS/haskell/hoglmv/.stack-work/install/x86_64-linux-tinfo6/a6d4fba4f9f76e5f2af2746d1d44995cf3678c60215993938dd0cec1bf7a477e/8.10.3/libexec/x86_64-linux-ghc-8.10.3/hoglmv-0.1.0.0"
sysconfdir = "/home/dave/Documents/CS/haskell/hoglmv/.stack-work/install/x86_64-linux-tinfo6/a6d4fba4f9f76e5f2af2746d1d44995cf3678c60215993938dd0cec1bf7a477e/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hoglmv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hoglmv_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hoglmv_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hoglmv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hoglmv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hoglmv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
