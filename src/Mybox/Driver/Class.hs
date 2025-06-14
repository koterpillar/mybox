module Mybox.Driver.Class where

import           Mybox.Prelude

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

class RunResultSimplified rr o | rr -> o where
  rrSimplify :: rr -> o

rrLower :: RunExit e -> RunOutput o -> RunResult ExitCode Text -> RunResult e o
rrLower re ro (RunResult e o) = RunResult (lowerE re e) (lowerO ro o)
  where
    lowerE :: RunExit e -> ExitCode -> e
    lowerE RunExitError _   = ()
    lowerE RunExitReturn ec = ec
    lowerO :: RunOutput o -> Text -> o
    lowerO RunOutputShow _        = ()
    lowerO RunOutputReturn output = output

instance RunResultSimplified (RunResult () ()) () where
  rrSimplify (RunResult () ()) = ()

instance RunResultSimplified (RunResult ExitCode ()) ExitCode where
  rrSimplify (RunResult exitCode ()) = exitCode

instance RunResultSimplified (RunResult () Text) Text where
  rrSimplify (RunResult () output) = output

type Args = NonEmpty Text

class Monad m =>
      MonadDriver m
  where
  drvRun_ ::
       RunExit e -- ^ Exit behavior
    -> RunOutput o -- ^ Output behavior
    -> Args -- ^ Command and arguments
    -> m (RunResult e o)

drvRun :: MonadDriver m => Args -> m ()
drvRun = fmap rrSimplify <$> drvRun_ RunExitError RunOutputShow

drvRunOk :: MonadDriver m => Args -> m ExitCode
drvRunOk = fmap rrSimplify <$> drvRun_ RunExitReturn RunOutputShow

drvRunOutput_ :: MonadDriver m => RunExit e -> Args -> m (RunResult e Text)
drvRunOutput_ exit = drvRun_ exit RunOutputReturn

drvRunOutput :: MonadDriver m => Args -> m Text
drvRunOutput = fmap rrSimplify <$> drvRunOutput_ RunExitError
