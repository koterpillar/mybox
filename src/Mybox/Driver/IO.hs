{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mybox.Driver.IO
  ( IODriver
  , localDriver
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)

import           Data.Has

import           Data.List.NonEmpty     (NonEmpty (..))

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text

import           Mybox.Driver.Class

import           System.Exit            (ExitCode (..))
import           System.Process         (readProcessWithExitCode)

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
        (exitCode, stdout, _stderr) <-
          liftIO $ readProcessWithExitCode cmd args ""
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

localDriver :: IODriver
localDriver = IODriver {idTransformArgs = id, idOverrides = mempty}
