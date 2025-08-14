{-# LANGUAGE TypeFamilies #-}

module Mybox.Driver.Class where

import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as Text
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
  RunOutputReturn :: RunOutput ByteString

data RunResult e o = RunResult
  { exit :: e
  , output :: o
  }
  deriving (Eq, Show)

rrMap :: (o -> o') -> RunResult e o -> RunResult e o'
rrMap f (RunResult e o) = RunResult e (f o)

class RunResultSimplified rr o | rr -> o where
  rrSimplify :: rr -> o

rrLower :: RunExit e -> RunOutput o -> RunResult ExitCode ByteString -> RunResult e o
rrLower re ro (RunResult e o) = RunResult (lowerE re e) (lowerO ro o)
 where
  lowerE :: RunExit e -> ExitCode -> e
  lowerE RunExitError _ = ()
  lowerE RunExitReturn ec = ec
  lowerO :: RunOutput o -> ByteString -> o
  lowerO RunOutputShow _ = ()
  lowerO RunOutputHide _ = ()
  lowerO RunOutputReturn output = output

instance RunResultSimplified (RunResult () ()) () where
  rrSimplify (RunResult () ()) = ()

instance RunResultSimplified (RunResult ExitCode ()) ExitCode where
  rrSimplify (RunResult exitCode ()) = exitCode

instance RunResultSimplified (RunResult () ByteString) ByteString where
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

drvRunOutputBinary_ :: Driver :> es => RunExit e -> Args -> Eff es (RunResult e ByteString)
drvRunOutputBinary_ exit = send . DrvRun exit RunOutputReturn

drvRunOutputBinary :: Driver :> es => Args -> Eff es ByteString
drvRunOutputBinary = fmap rrSimplify <$> drvRunOutputBinary_ RunExitError

drvRunOutputExitBinary :: Driver :> es => Args -> Eff es (RunResult ExitCode ByteString)
drvRunOutputExitBinary = drvRunOutputBinary_ RunExitReturn

drvRunOutput :: Driver :> es => Args -> Eff es Text
drvRunOutput = fmap Text.decodeUtf8 . drvRunOutputBinary

drvRunOutputExit :: Driver :> es => Args -> Eff es (RunResult ExitCode Text)
drvRunOutputExit = fmap (rrMap Text.decodeUtf8) . drvRunOutputExitBinary
