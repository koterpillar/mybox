{-# LANGUAGE GADTs #-}

module Driver.Test
  ( drvLocalTest
  , drvDocker
  , drvDisableRoot
  ) where

import           Data.Function        ((&))

import           Data.Maybe

import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

import           System.Directory
import           System.Environment
import           System.IO.Temp
import           System.Process.Typed hiding (Inherit)

import           System.Random

import           Driver

import           Paths_mybox

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

drvDisableRoot :: Driver -> Driver
drvDisableRoot =
  drvModify $ \originalRun root ro ->
    if root
      then error $
           Text.unpack $ "Root operations are disabled: " <> showCommand ro
      else originalRun False ro

overrideHome :: FilePath -> Text -> DriverRun -> DriverRun
overrideHome home path originalRun root ro =
  originalRun root $
  roPrependArgs
    [ "env"
    , "HOME=" <> Text.pack home
    , "PATH=" <> Text.pack home <> "/.local/bin:" <> path
    ]
    ro

drvLocalTest :: IO Driver -- FIXME: clean up temporary directory
drvLocalTest = do
  tmp <- getCanonicalTemporaryDirectory
  home <- createTempDirectory tmp "mybox-home"
  path <- Text.pack . fromMaybe "" <$> lookupEnv "PATH"
  pure $ drvLogging $ drvModify (overrideHome home path) drvLocal

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

dockerUser :: Text
dockerUser = "regular_user"

dockerContainerPrefix :: Text
dockerContainerPrefix = "mybox-test-"

drvDocker :: Text -> IO Driver
drvDocker baseImage = do
  packageRoot <- getDataDir
  let bootstrap = packageRoot <> "/bootstrap"
  let image = dockerImagePrefix <> baseImage
  let dockerfile =
        Text.unlines
          [ "FROM " <> image
          , "RUN useradd --create-home --password '' " <> dockerUser
          , "COPY bootstrap /bootstrap"
          , "RUN /bootstrap --development"
          , "ENV PATH /home/" <> dockerUser <> "/.local/bin:$PATH"
          , "USER " <> dockerUser
          , "RUN command -v dnf >/dev/null && dnf check-update || true" -- populate dnf cache so each test doesn't have to do it
          ]
  withSystemTempDirectory "mybox-docker" $ \dockerRoot -> do
    copyFile bootstrap $ dockerRoot <> "/bootstrap"
    runProcess_ $
      procText ("docker" :| ["build", "--tag", image, Text.pack dockerRoot]) &
      setStdin (byteStringInput $ procEncode dockerfile)
  container <- (dockerContainerPrefix <>) . Text.pack . show @Int <$> randomIO
  runProcess_
    (proc
       "docker"
       [ "run"
       , "--rm"
       , "--detach"
       , "--volume"
       , packageRoot <> ":" <> packageRoot
       , "--name"
       , Text.unpack container
       , Text.unpack image
       , "sleep"
       , "86400000"
       ])
  pure $ drvLogging $ drvMake $ dockerRun container

dockerRun :: Text -> DriverRun
dockerRun container root ro = drvProcessRun $ roPrependArgs dockerArgs ro
  where
    dockerArgs =
      ["docker", "exec"] ++
      (if root
         then ["--user", "root"]
         else []) ++
      ["--interactive", container]
