module Package
  ( Package(..)
  , pkIsInstalled
  ) where

import Data.Text (Text)

import           Driver

class Package package where
  pkInstall :: Driver -> package -> IO ()
  pkRemoteVersion :: Driver -> package -> IO Text
  pkLocalVersion :: Driver -> package -> IO Text

pkIsInstalled :: Package package => Driver -> package -> IO Bool
pkIsInstalled driver package = do
  remote <- pkRemoteVersion driver package
  local <- pkLocalVersion driver package
  pure $ remote == local
