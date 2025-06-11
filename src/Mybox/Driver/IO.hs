{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mybox.Driver.IO
  ( IODriver
  , localDriver
  , dockerDriver
  ) where

import           Control.Exception.Safe (MonadMask)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks, runReaderT)

import           Data.Foldable

import           Data.Has

import           Data.List.NonEmpty     (NonEmpty (..))

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text

import           Mybox.Driver.Class
import           Mybox.Driver.Ops

import           System.Exit            (ExitCode (..))
import           System.Process         (readProcessWithExitCode)
import           System.Random

data IODriver = IODriver
  { idTransformArgs :: Args -> Args
  , idOverrides     :: Map Args (RunResult ExitCode Text)
  }

instance (Monad m, MonadIO m, Has IODriver r, MonadReader r m) => MonadDriver m where
  drvRun_ exitBehavior outputBehavior args_ = do
    override <- asks $ Map.lookup args_ . idOverrides . getter
    case override of
      Just result -> pure $ rrLower exitBehavior outputBehavior result
      Nothing -> do
        (cmd :| args) <-
          asks $ fmap Text.unpack . ($ args_) . idTransformArgs . getter
        (exitCode, stdout, stderr) <-
          liftIO $ readProcessWithExitCode cmd args ""
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
localDriver = IODriver {idTransformArgs = id, idOverrides = mempty}

-- FIXME: Cleanup or ResourceT
dockerDriver ::
     forall m. (MonadIO m, MonadMask m)
  => Text -- ^ Base image
  -> m IODriver
dockerDriver baseImage = do
  let localRun ::
           (forall n. (MonadDriver n, MonadIO n, MonadMask n) => n a) -> m a
      localRun action = runReaderT action localDriver
  containerId <- Text.pack . show <$> liftIO (randomIO @Int)
  container <-
    localRun
      $ drvTempDir
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
                   , "mybox-test-" <> containerId
                   , image
                   , "sleep"
                   , "86400000"
                   ]
  let idTransformArgs :: Args -> Args
      idTransformArgs args =
        "docker" :| ["exec", "--interactive", container] <> toList args
  pure IODriver {idTransformArgs, idOverrides = mempty}

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
