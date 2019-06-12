module Paths_eta_repl (
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
libdir     = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/lib/x86_64-linux-ghc-7.10.3/eta-repl-0.8.6.5-F66JYDRSgIVJhcgi9hwgeO"
datadir    = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/share/x86_64-linux-ghc-7.10.3/eta-repl-0.8.6.5"
libexecdir = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/libexec"
sysconfdir = "/projects/eta/.stack-work/install/x86_64-linux/lts-6.27/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eta_repl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eta_repl_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "eta_repl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eta_repl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eta_repl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
