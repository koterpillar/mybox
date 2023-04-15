module Driver.Actions
  ( drvExecutableExists
  , drvFindExecutable
  , drvHome
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Driver

drvExecutableExists :: Text -> Driver -> IO Bool
drvExecutableExists name = drvRunOK $ shellCmd $ "command -v " <> name

drvFindExecutable :: [Text] -> Driver -> IO Text
drvFindExecutable = error "drvFindExecutable: not implemented"

drvHome :: Driver -> IO FilePath
drvHome drv = do
  let path =
        if drvRoot drv
          then "~root"
          else "~"
  Text.unpack <$> drvRunOutput (shellCmd $ "eval echo " <> path) drv
