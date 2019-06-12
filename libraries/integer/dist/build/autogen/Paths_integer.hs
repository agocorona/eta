{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_integer (
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
version = Version [0,5,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.etlas/bin"
libdir     = "/root/.etlas/lib/eta-0.8.6.5/integer-0.5.1.0-ACyqTmAMUMGGvisLFeQbAm"
dynlibdir  = "/root/.etlas/lib/eta-0.8.6.5"
datadir    = "/root/.etlas/share/eta-0.8.6.5/integer-0.5.1.0"
libexecdir = "/root/.etlas/libexec"
sysconfdir = "/root/.etlas/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "integer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "integer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "integer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "integer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "integer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "integer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
