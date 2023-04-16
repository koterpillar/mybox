module Package.Clone
  ( Clone(..)
  ) where

import           Data.Text (Text)

import           Package

data Clone =
  Clone
    { cloneRepo        :: Text
    , cloneDestination :: FilePath
    }
  deriving (Eq, Show)

instance Package Clone where
  pkRemoteVersion _ _ = error "pkRemoteVersion for Clone: not implemented"
  pkLocalVersion _ _ = error "pkLocalVersion for Clone: not implemented"
  pkInstall _ _ = error "pkInstall for Clone: not implemented"
