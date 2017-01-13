{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_compiler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\study_files\\3-autumn\\Haskell\\hw\\final\\compiler\\.stack-work\\install\\8b29d029\\bin"
libdir     = "D:\\study_files\\3-autumn\\Haskell\\hw\\final\\compiler\\.stack-work\\install\\8b29d029\\lib\\x86_64-windows-ghc-8.0.1\\compiler-0.1.0.0-I0pxvr0RakAFj74FxK0lcc"
datadir    = "D:\\study_files\\3-autumn\\Haskell\\hw\\final\\compiler\\.stack-work\\install\\8b29d029\\share\\x86_64-windows-ghc-8.0.1\\compiler-0.1.0.0"
libexecdir = "D:\\study_files\\3-autumn\\Haskell\\hw\\final\\compiler\\.stack-work\\install\\8b29d029\\libexec"
sysconfdir = "D:\\study_files\\3-autumn\\Haskell\\hw\\final\\compiler\\.stack-work\\install\\8b29d029\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "compiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "compiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "compiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "compiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
