module Installer where

data Installer =
  Installer
    { doInstall   :: IO ()
    , isInstalled :: IO Bool
    }
