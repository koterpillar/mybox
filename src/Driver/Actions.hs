module Driver.Actions
  ( drvExecutableExists
  , drvFindExecutable
  , drvHome
  , drvIsDirectory
  , drvIsFile
  , drvIsExecutable
  , shellCmd
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Driver.Types

shellCmd :: Text -> NonEmpty Text
shellCmd cmd = "sh" :| ["-c", cmd]

drvExecutableExists :: Text -> Driver -> IO Bool
drvExecutableExists name = drvRunOK $ shellCmd $ "command -v " <> name

drvFindExecutable :: [Text] -> Driver -> IO Text
drvFindExecutable = error "drvFindExecutable: not implemented"

drvHome :: Driver -> IO FilePath
drvHome drv = Text.unpack <$> drvRunOutput (shellCmd "eval echo ~") drv

drvTest :: Text -> Text -> Driver -> IO Bool
drvTest test arg = drvRunOK ("test" :| [test, arg])

drvIsFile :: FilePath -> Driver -> IO Bool
drvIsFile = drvTest "-f" . Text.pack

drvIsExecutable :: FilePath -> Driver -> IO Bool
drvIsExecutable = drvTest "-x" . Text.pack

drvIsDirectory :: FilePath -> Driver -> IO Bool
drvIsDirectory = drvTest "-d" . Text.pack
