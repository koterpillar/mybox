{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mybox.Driver.IO
  ( ProcessDriver
  , localDriver
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)

import           Data.Has

import           Data.List.NonEmpty     (NonEmpty (..))

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text

import           Mybox.Driver.Class

import           System.Exit            (ExitCode (..))
import           System.Process         (readProcessWithExitCode)

newtype ProcessDriver = ProcessDriver
  { pdTransformArgs :: NonEmpty Text -> NonEmpty Text
  }

localDriver :: ProcessDriver
localDriver = ProcessDriver {pdTransformArgs = id}

instance (Monad m, MonadIO m, Has ProcessDriver r, MonadReader r m) =>
         MonadDriver m where
  drvRun_ exitBehavior outputBehavior args_ = do
    (cmd :| args) <-
      asks $ fmap Text.unpack . ($ args_) . pdTransformArgs . getter
    (exitCode, stdout, _stderr) <- liftIO $ readProcessWithExitCode cmd args ""
    let stdoutText = Text.pack stdout
    rrExit <-
      case exitBehavior of
        RunExitError ->
          case exitCode of
            ExitSuccess -> pure ()
            ExitFailure code ->
              error $ "Process failed with exit code: " ++ show code
        RunExitReturn -> pure exitCode
    rrOutput <-
      case outputBehavior of
        RunOutputShow   -> void $ liftIO $ Text.putStr stdoutText
        RunOutputReturn -> pure $ Text.strip stdoutText
    pure $ RunResult {..}
