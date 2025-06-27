{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mybox.Driver.IO
  ( IODriver
  , localDriver
  , testDriver
  ) where

import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text

import           Effectful.Dispatch.Dynamic
import           Effectful.Exception

import           Mybox.Driver.Class
import           Mybox.Driver.Ops
import           Mybox.Prelude

import           System.Environment
import           System.Process
import           System.Random

data IODriver = IODriver
  { transformArgs :: Args -> Args
  , cwd           :: Maybe Text
  }

runDriverIO :: IOE :> es => IODriver -> Eff (Driver : es) a -> Eff es a
runDriverIO drv =
  interpret_ $ \case
    DrvRun exitBehavior outputBehavior args_ -> do
      let (cmd :| args) = Text.unpack <$> drv.transformArgs args_
      let process = (proc cmd args) {System.Process.cwd = Text.unpack <$> drv.cwd}
      (exitCode, stdout, stderr) <-
        liftIO $ readCreateProcessWithExitCode process ""
      let stdoutText = Text.pack stdout
      exit <-
        case exitBehavior of
          RunExitError ->
            case exitCode of
              ExitSuccess -> pure ()
              ExitFailure code ->
                error
                  $ "Process "
                      <> Text.unpack (shellJoin args_)
                      <> " failed with exit code: "
                      ++ show code
                      ++ " and stderr: "
                      ++ stderr
          RunExitReturn -> pure exitCode
      output <-
        case outputBehavior of
          RunOutputShow   -> void $ liftIO $ Text.putStr stdoutText
          RunOutputReturn -> pure $ Text.strip stdoutText
      pure $ RunResult {..}

localDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
localDriver = runDriverIO $ IODriver {transformArgs = id, cwd = Nothing}

withAddedPath :: IOE :> es => Text -> Eff es a -> Eff es a
withAddedPath addPath action = do
  originalPath <- fmap Text.pack $ liftIO $ getEnv "PATH"
  let newPath = addPath <> ":" <> originalPath
  bracket_
    (liftIO $ setEnv "PATH" $ Text.unpack newPath)
    (liftIO $ setEnv "PATH" $ Text.unpack originalPath)
    action

testHostDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testHostDriver act = do
  originalHome <- fmap Text.pack $ liftIO $ getEnv "HOME"
  bracket (localDriver drvTempDir) (\home -> localDriver $ drvRm home) $ \home -> do
    withAddedPath (home </> ".local" </> "bin") $ do
      let linkToOriginalHome path =
            let op = originalHome </> path
                np = home </> path
             in localDriver $ do
                  drvMkdir $ pDirname np
                  drvLink op np
      linkToOriginalHome ".local/share/fonts"
      linkToOriginalHome ".local/share/systemd/user"
      linkToOriginalHome "Library/LaunchAgents"
      let transformArgs ("sh" :| ["-c", "eval echo '~'"]) = "echo" :| [home]
          transformArgs args                              = args
      let cwd = Just home
      flip runDriverIO act $ IODriver {..}

dockerDriver :: IOE :> es => Text -> Eff (Driver : es) a -> Eff es a
dockerDriver baseImage act =
  bracket mkContainer rmContainer (flip runDriverIO act . mkDriver)
  where
    mkContainer :: IOE :> es => Eff es Text
    mkContainer =
      localDriver $ do
        containerName <- Text.pack . show <$> liftIO (randomIO @Int)
        bracket drvTempDir drvRm $ \tempDir -> do
          drvCopy "bootstrap" (tempDir </> "bootstrap")
          drvWriteFile (tempDir </> "Dockerfile") $ dockerfile baseImage
          let image = dockerImagePrefix <> baseImage
          drvRun $ "docker" :| ["build", "--tag", image, tempDir]
          -- FIXME: --volume for package root
          drvRunOutput
            $ "docker"
                :| [ "run"
                   , "--rm"
                   , "--detach"
                   , "--name"
                   , "mybox-test-" <> containerName
                   , image
                   , "sleep"
                   , "86400000"
                   ]
    rmContainer :: IOE :> es => Text -> Eff es ()
    rmContainer container =
      localDriver $ drvRun $ "docker" :| ["rm", "--force", container]
    mkDriver :: Text -> IODriver
    mkDriver container = IODriver {..}
      where
        cwd = Nothing
        transformArgs :: Args -> Args
        transformArgs ("sudo" :| args') =
          "docker" :| ["exec", "--user", "root", "--interactive", container]
            <> args'
        transformArgs args =
          "docker" :| ["exec", "--interactive", container] <> toList args

dockerfile ::
     Text -- ^ base image
  -> Text
dockerfile baseImage =
  Text.unlines
    [ "FROM " <> baseImage
    , "RUN useradd --create-home --password '' " <> dockerUser
    , "COPY bootstrap /bootstrap"
    , "RUN /bootstrap --development"
    , "ENV PATH=/home/{DOCKER_USER}/.local/bin:$PATH"
    , "USER " <> dockerUser
    -- populate dnf cache so each test doesn't have to do it
    , "RUN command -v dnf >/dev/null && dnf check-update || true"
    ]

dockerUser :: Text
dockerUser = "regular_user"

dockerImagePrefix :: Text
dockerImagePrefix = "mybox-test-"

testDriver :: IOE :> es => Eff (Driver : es) a -> Eff es a
testDriver act = do
  image_ <- liftIO $ lookupEnv "DOCKER_IMAGE"
  case image_ of
    Nothing    -> testHostDriver act
    Just image -> dockerDriver (Text.pack image) act
