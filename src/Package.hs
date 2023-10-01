module Package
  ( Package(..)
  , pkIsInstalled
  ) where

import           Data.Text (Text)

import           Driver
import           State     (Connection)

class Package package where
  pkInstall :: Connection -> Driver -> package -> IO ()
  pkRemoteVersion :: Driver -> package -> IO Text
  pkLocalVersion :: Connection -> Driver -> package -> IO (Maybe Text)

pkIsInstalled :: Package package => Connection -> Driver -> package -> IO Bool
pkIsInstalled db driver package = do
  remote <- pkRemoteVersion driver package
  local <- pkLocalVersion db driver package
  pure $ local == Just remote
