{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_rts (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.etlas/bin"
libdir     = "/root/.etlas/lib/eta-0.8.6.5/rts-0.1.0.0-5Dj4qf1Wx9cCeYbpni5T1w"
dynlibdir  = "/root/.etlas/lib/eta-0.8.6.5"
datadir    = "/root/.etlas/share/eta-0.8.6.5/rts-0.1.0.0"
libexecdir = "/root/.etlas/libexec"
sysconfdir = "/root/.etlas/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rts_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rts_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rts_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rts_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rts_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rts_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
