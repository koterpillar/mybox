module Package.ManualVersion
  ( ManualVersionPackage(..)
  , ManualVersionPK(..)
  ) where

import           Data.Text     (Text)

import           Driver
import           State
import           State.Version

import           Package

class ManualVersionPackage package where
  mvpkInstall :: Connection -> Driver -> package -> IO ()
  mvpkRemoteVersion :: Driver -> package -> IO Text

newtype ManualVersionPK package =
  ManualVersionPK package

instance ManualVersionPackage package => Package (ManualVersionPK package) where
  pkInstall connection driver (ManualVersionPK package) = do
    version <- mvpkRemoteVersion driver package
    mvpkInstall connection driver package
    error $
      "pkInstall for ManualVersionPackage: record version=" ++ show version
  pkRemoteVersion driver (ManualVersionPK package) =
    mvpkRemoteVersion driver package
  pkLocalVersion connection _ (ManualVersionPK package) = do
    vs <- versions connection
    v <-
      storeGet
        (error "pkLocalVersion for ManualVersionPackage: package name")
        vs
    pure $ fmap vVersion v
