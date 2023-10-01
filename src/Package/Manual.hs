module Package.Manual
  ( ManualPackage(..)
  , MPK(..)
  ) where

import           Data.Text (Text)

import           Driver

import           Package

class ManualPackage package where
  mpkInstall :: Driver -> package -> IO ()
  mpkRemoteVersion :: Driver -> package -> IO Text

newtype MPK package =
  MPK package

instance ManualPackage package => Package (MPK package) where
  pkInstall driver (MPK package) = do
    version <- mpkRemoteVersion driver package
    mpkInstall driver package
    error $ "pkInstall for ManualPackage: record version=" ++ show version
  pkRemoteVersion driver (MPK package) = mpkRemoteVersion driver package
  pkLocalVersion = error "pkLocalVersion for ManualPackage: not implemented"
