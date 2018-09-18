{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_research (
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

bindir     = "/Users/uchidamizuki/github/research/.stack-work/install/x86_64-osx/lts-12.0/8.4.3/bin"
libdir     = "/Users/uchidamizuki/github/research/.stack-work/install/x86_64-osx/lts-12.0/8.4.3/lib/x86_64-osx-ghc-8.4.3/research-0.1.0.0-F1MtuIHItpp8AFGHBe1k9V-research-exe"
dynlibdir  = "/Users/uchidamizuki/github/research/.stack-work/install/x86_64-osx/lts-12.0/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/uchidamizuki/github/research/.stack-work/install/x86_64-osx/lts-12.0/8.4.3/share/x86_64-osx-ghc-8.4.3/research-0.1.0.0"
libexecdir = "/Users/uchidamizuki/github/research/.stack-work/install/x86_64-osx/lts-12.0/8.4.3/libexec/x86_64-osx-ghc-8.4.3/research-0.1.0.0"
sysconfdir = "/Users/uchidamizuki/github/research/.stack-work/install/x86_64-osx/lts-12.0/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "research_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "research_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "research_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "research_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "research_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "research_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
