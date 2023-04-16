module Package.Clone where

import           Data.Text (Text)

import           Package

data Clone =
  Clone
    { cloneRepo        :: Text
    , cloneDestination :: FilePath
    }
  deriving (Eq, Show)

instance Package Clone
