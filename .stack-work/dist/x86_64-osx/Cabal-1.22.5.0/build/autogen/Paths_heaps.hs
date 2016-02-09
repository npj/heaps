module Paths_heaps (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/npj/Personal/projects/haskell/heaps/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/bin"
libdir     = "/Users/npj/Personal/projects/haskell/heaps/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/heaps-0.1.0.0-D84gs2t4foVLGrEFu7URZk"
datadir    = "/Users/npj/Personal/projects/haskell/heaps/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/share/x86_64-osx-ghc-7.10.3/heaps-0.1.0.0"
libexecdir = "/Users/npj/Personal/projects/haskell/heaps/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/libexec"
sysconfdir = "/Users/npj/Personal/projects/haskell/heaps/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "heaps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "heaps_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "heaps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "heaps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "heaps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
