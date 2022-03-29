module Tool where

import           Data.Text (Text)

import           Installer

data Tool =
  Tool
    { tName         :: Text
    , tDependencies :: [Text]
    , tCategories   :: [Text]
    , tInstaller    :: Installer
    }
  deriving (Show)
