module Mybox.Driver.Class where

import Data.List.NonEmpty (NonEmpty(..))
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

class RunResultSimplified rr o | rr -> o where rrSimplify :: rr -> o

instance RunResultSimplified (RunResult () ()) () where
  rrSimplify (RunResult () ()) = ()

instance RunResultSimplified (RunResult ExitCode ()) ExitCode where
  rrSimplify (RunResult exitCode ()) = exitCode

instance RunResultSimplified (RunResult () Text) Text where
  rrSimplify (RunResult () output) = output

class Monad m =>
      MonadDriver m
  where
  drvRun_ ::
       RunExit e -- ^ Exit behavior
    -> RunOutput o -- ^ Output behavior
    -> NonEmpty Text -- ^ Command and arguments
    -> m (RunResult e o)

drvRun :: MonadDriver m => NonEmpty Text -> m ()
drvRun = fmap rrSimplify <$> drvRun_ RunExitError RunOutputShow

drvRunOk :: MonadDriver m => NonEmpty Text -> m ExitCode
drvRunOk = fmap rrSimplify <$> drvRun_ RunExitReturn RunOutputShow

drvRunOutput_ ::
     MonadDriver m => RunExit e -> NonEmpty Text -> m (RunResult e Text)
drvRunOutput_ exit = drvRun_ exit RunOutputReturn

drvRunOutput :: MonadDriver m => NonEmpty Text -> m Text
drvRunOutput = fmap rrSimplify <$> drvRunOutput_ RunExitError
