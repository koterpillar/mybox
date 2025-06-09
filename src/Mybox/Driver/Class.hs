module Mybox.Driver.Class where

import           Data.Text   (Text)

import           System.Exit (ExitCode)

data RunExit e where
  RunExitError :: RunExit () -- ^ Throw an error if the command fails
  RunExitReturn :: RunExit ExitCode -- ^ Return the exit code

data RunOutput o where
  RunOutputShow :: RunOutput ()
  RunOutputReturn :: RunOutput Text

data RunResult e o = RunResult
  { rrExit   :: e
  , rrOutput :: o
  } deriving (Show, Eq)

class Monad m =>
      MonadDriver m
  where
  drvRun_ ::
       RunExit e -- ^ Exit behavior
    -> RunOutput o -- ^ Output behavior
    -> [Text] -- ^ Command and arguments
    -> m (RunResult e o)

drvRun :: MonadDriver m => [Text] -> m ()
drvRun cmd = do
  RunResult () () <- drvRun_ RunExitError RunOutputShow cmd
  pure ()

drvRunOk :: MonadDriver m => [Text] -> m ExitCode
drvRunOk cmd = do
  RunResult rrExit () <- drvRun_ RunExitReturn RunOutputShow cmd
  pure rrExit

drvRunOutput_ ::
     MonadDriver m => RunExit e -> [Text] -> m (RunResult e Text)
drvRunOutput_ exit = drvRun_ exit RunOutputReturn

drvRunOutput :: MonadDriver m => [Text] -> m Text
drvRunOutput cmd = do
  RunResult () output <- drvRunOutput_ RunExitError cmd
  pure output
