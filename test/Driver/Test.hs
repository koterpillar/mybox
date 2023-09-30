{-# LANGUAGE GADTs #-}

module Driver.Test
  ( drvLocalTest
  , drvDocker
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
import Driver.Types (roPrependArgs)

import Process

import           Paths_mybox

drvLogging :: Driver -> Driver
drvLogging (Driver originalRun) =
  Driver $ \ro -> do
    Text.putStrLn $ "->" <> commandPrefix False <> " " <> showCommand ro
    originalRun ro

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

overrideHome :: FilePath -> Text -> Driver -> Driver
overrideHome home path (Driver originalRun) =
  Driver $ \ro -> originalRun $
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
  pure $ drvLogging $ overrideHome home path $ drvProcess

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
  pure $ drvLogging $ dockerRun container

dockerRun :: Text -> Driver
dockerRun container = Driver $ \ro -> originalRun $ roPrependArgs dockerArgs ro
  where
    Driver originalRun = drvProcess
    dockerArgs =
      ["docker", "exec", "--interactive", container]
