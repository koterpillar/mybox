module Tool where

import           Data.Text (Text)

import           Installer

data Tool =
  Tool
    { name         :: Text
    , dependencies :: [Text]
    , categories   :: [Text]
    , installer    :: Installer
    }

instance Show Tool where
  show Tool {..} =
    "Tool { name = " ++
    show name ++
    ", dependencies = " ++
    show dependencies ++
    ", categories = " ++ show categories ++ ", installer = ...}"
