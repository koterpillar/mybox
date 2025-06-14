{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mybox.Driver.IO
  ( IODriver
  , localDriver
  , testHostDriver
  , dockerDriver
  ) where

import           Control.Monad.Reader (MonadReader, asks, runReaderT)

import           Data.Has

import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

import           Mybox.Driver.Class
import           Mybox.Driver.Ops
import           Mybox.Prelude

import           System.Environment
import           System.Process
import           System.Random

data IODriver = IODriver
  { idTransformArgs :: Args -> Args
  , idCwd           :: Maybe Text
  }

instance (Monad m, MonadIO m, Has IODriver r, MonadReader r m) => MonadDriver m where
  drvRun_ exitBehavior outputBehavior args_ = do
    (cmd :| args) <-
      asks $ fmap Text.unpack . ($ args_) . idTransformArgs . getter
    cwd <- asks $ idCwd . getter
    let process = (proc cmd args) {cwd = Text.unpack <$> cwd}
    (exitCode, stdout, stderr) <-
      liftIO $ readCreateProcessWithExitCode process ""
    let stdoutText = Text.pack stdout
    rrExit <-
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
    rrOutput <-
      case outputBehavior of
        RunOutputShow   -> void $ liftIO $ Text.putStr stdoutText
        RunOutputReturn -> pure $ Text.strip stdoutText
    pure $ RunResult {..}

localDriver :: IODriver
localDriver = IODriver {idTransformArgs = id, idCwd = Nothing}

localRun ::
     (MonadIO m, MonadMask m)
  => (forall n. (MonadDriver n, MonadIO n, MonadMask n) => n a)
  -> m a
localRun action = runReaderT action localDriver

withAddedPath :: (MonadIO m, MonadMask m) => Text -> m a -> m a
withAddedPath addPath action = do
  originalPath <- fmap Text.pack $ liftIO $ getEnv "PATH"
  let newPath = addPath <> ":" <> originalPath
  bracket_
    (liftIO $ setEnv "PATH" $ Text.unpack newPath)
    (liftIO $ setEnv "PATH" $ Text.unpack originalPath)
    action

testHostDriver ::
     forall m a. (MonadIO m, MonadMask m)
  => (IODriver -> m a)
  -> m a
testHostDriver act = do
  originalHome <- fmap Text.pack $ liftIO $ getEnv "HOME"
  bracket (localRun drvTempDir) (\home -> localRun $ drvRm home) $ \home -> do
    withAddedPath (home <> "/.local/bin") $ do
      let linkToOriginalHome path =
            let op = originalHome <> "/" <> path
                np = home <> "/" <> path
             in localRun $ do
                  drvMkdir $ drvDirname np
                  drvLink op np
      linkToOriginalHome ".local/share/fonts"
      linkToOriginalHome ".local/share/systemd/user"
      linkToOriginalHome "Library/LaunchAgents"
      let idTransformArgs ("sh" :| ["-c", "eval echo '~'"]) = "echo" :| [home]
          idTransformArgs args                              = args
      let idCwd = Just home
      act $ IODriver {..}

dockerDriver ::
     forall m a. (MonadIO m, MonadMask m)
  => Text -- ^ Base image
  -> (IODriver -> m a)
  -> m a
dockerDriver baseImage act = bracket mkContainer rmContainer (act . mkDriver)
  where
    mkContainer :: m Text
    mkContainer = do
      containerName <- Text.pack . show <$> liftIO (randomIO @Int)
      localRun
        $ bracket drvTempDir drvRm
        $ \tempDir -> do
            drvCopy "bootstrap" (tempDir <> "/bootstrap")
            drvWriteFile (tempDir <> "/Dockerfile") $ dockerfile baseImage
            let image = dockerImagePrefix <> baseImage
                  -- Build image
            drvRun $ "docker" :| ["build", "--tag", image, tempDir]
                  -- Start container
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
    rmContainer :: Text -> m ()
    rmContainer container =
      localRun $ drvRun $ "docker" :| ["rm", "--force", container]
    mkDriver :: Text -> IODriver
    mkDriver container = IODriver {..}
      where
        idCwd = Nothing
        idTransformArgs :: Args -> Args
        idTransformArgs args =
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
