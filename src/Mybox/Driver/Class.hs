{-# LANGUAGE TypeFamilies #-}

module Mybox.Driver.Class where

import Effectful.Dispatch.Dynamic

import Mybox.Prelude

data RunExit e where
  RunExitError ::
    -- | Throw an error if the command fails
    RunExit ()
  RunExitReturn ::
    -- | Return the exit code
    RunExit ExitCode

data RunOutput o where
  RunOutputShow :: RunOutput ()
  RunOutputHide :: RunOutput ()
  RunOutputReturn :: RunOutput Text

data RunResult e o = RunResult
  { exit :: e
  , output :: o
  }
  deriving (Eq, Show)

class RunResultSimplified rr o | rr -> o where
  rrSimplify :: rr -> o

rrLower :: RunExit e -> RunOutput o -> RunResult ExitCode Text -> RunResult e o
rrLower re ro (RunResult e o) = RunResult (lowerE re e) (lowerO ro o)
 where
  lowerE :: RunExit e -> ExitCode -> e
  lowerE RunExitError _ = ()
  lowerE RunExitReturn ec = ec
  lowerO :: RunOutput o -> Text -> o
  lowerO RunOutputShow _ = ()
  lowerO RunOutputHide _ = ()
  lowerO RunOutputReturn output = output

instance RunResultSimplified (RunResult () ()) () where
  rrSimplify (RunResult () ()) = ()

instance RunResultSimplified (RunResult ExitCode ()) ExitCode where
  rrSimplify (RunResult exitCode ()) = exitCode

instance RunResultSimplified (RunResult () Text) Text where
  rrSimplify (RunResult () output) = output

type Args = NonEmpty Text

data Driver :: Effect where
  DrvRun ::
    -- | Exit behavior
    RunExit e ->
    -- | Output behavior
    RunOutput o ->
    -- | Command and arguments
    Args ->
    Driver m (RunResult e o)

type instance DispatchOf Driver = Dynamic

drvRun :: Driver :> es => Args -> Eff es ()
drvRun = fmap rrSimplify . send . DrvRun RunExitError RunOutputShow

drvRunOk :: Driver :> es => Args -> Eff es ExitCode
drvRunOk = fmap rrSimplify . send . DrvRun RunExitReturn RunOutputShow

drvRunSilent :: Driver :> es => Args -> Eff es ()
drvRunSilent = fmap rrSimplify . send . DrvRun RunExitError RunOutputHide

drvRunOutput_ :: Driver :> es => RunExit e -> Args -> Eff es (RunResult e Text)
drvRunOutput_ exit = send . DrvRun exit RunOutputReturn

drvRunOutput :: Driver :> es => Args -> Eff es Text
drvRunOutput = fmap rrSimplify <$> drvRunOutput_ RunExitError

drvRunOutputExit :: Driver :> es => Args -> Eff es (RunResult ExitCode Text)
drvRunOutputExit = drvRunOutput_ RunExitReturn
