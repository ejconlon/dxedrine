{-# LANGUAGE CPP #-}

module Dxedrine.Paths
  ( getStaticDir
  ) where

#if defined(CABAL)
import System.FilePath ((</>))
import Paths_dxedrine (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = fmap getDataDir (</> "static")
#else
getStaticDir :: IO FilePath
getStaticDir = return "static"
#endif
