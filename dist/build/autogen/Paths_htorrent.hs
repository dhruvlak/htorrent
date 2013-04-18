module Paths_htorrent (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mgenius/bin"
libdir     = "/home/mgenius/lib/htorrent-0.1.0.0/ghc-7.4.1"
datadir    = "/home/mgenius/share/htorrent-0.1.0.0"
libexecdir = "/home/mgenius/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "htorrent_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "htorrent_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "htorrent_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "htorrent_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
