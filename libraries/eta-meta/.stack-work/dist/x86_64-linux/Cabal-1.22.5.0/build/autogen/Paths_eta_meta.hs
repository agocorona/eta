module Paths_eta_meta (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,8,6,5] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/bin"
libdir     = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/lib/x86_64-linux-ghc-7.10.3/eta-meta-0.8.6.5-1voU9yiB6Lc23r984r7e5E"
datadir    = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/share/x86_64-linux-ghc-7.10.3/eta-meta-0.8.6.5"
libexecdir = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/libexec"
sysconfdir = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eta_meta_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eta_meta_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "eta_meta_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eta_meta_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eta_meta_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
