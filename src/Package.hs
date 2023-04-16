module Package
  ( Package(..)
  ) where

import           Driver

class Package package where
  pkInstall :: Driver -> package -> IO ()
  pkIsInstalled :: Driver -> package -> IO Bool
