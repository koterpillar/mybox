module Driver.Test
  ( drvLocalTest
  , drvDocker
  ) where

import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import           Driver

drvLogging :: Driver -> Driver
drvLogging =
  drvModify $ \originalRun root ro -> do
    Text.putStrLn $ "->" <> commandPrefix root <> " " <> showCommand ro
    originalRun root ro

commandPrefix :: Bool -> Text
commandPrefix True  = "#"
commandPrefix False = "$"

showArg :: Text -> Text
showArg t
  | not $ Text.elem ' ' t = t
  | not $ Text.elem '"' t = "\"" <> t <> "\""
  | otherwise = "'" <> Text.replace "'" "'\"'\"'" t <> "'"

showCommand :: RunOptions output -> Text
showCommand RunOptions {roArgs = command :| args} =
  Text.unwords $ map showArg (command : args)

drvDisableRoot :: Bool -> Driver -> Driver
drvDisableRoot False = id
drvDisableRoot True =
  drvModify $ \originalRun root ro ->
    if root
      then error $
           Text.unpack $ "Root operations are disabled: " <> showCommand ro
      else originalRun False ro

drvLocalTest :: Bool -> Driver
drvLocalTest disable =
  drvLogging $
  drvDisableRoot disable $ error "Not implemented: OverrideHomeDriver + PATH"

drvDocker :: Text -> Driver
drvDocker _ = drvLogging $ error "Not implemented: DockerDriver"
