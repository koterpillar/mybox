module Mybox.Display.Print (
  Print,
  run,
  runPure,
  print,
  printLn,
  flush,
) where

import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared
import System.IO hiding (print)
import Prelude hiding (print)

import Mybox.Prelude

data Print :: Effect where
  Print :: String -> Print m ()
  Flush :: Print m ()

type instance DispatchOf Print = Dynamic

run :: IOE :> es => Handle -> Eff (Print : es) r -> Eff es r
run h = interpret_ $
  \case
    Print str -> liftIO $ hPutStr h str
    Flush -> liftIO $ hFlush h

runPure :: Eff (Print : es) r -> Eff es (r, String)
runPure act = do
  (r, logs) <- reinterpretWith_ (runState @[String] mempty) act $
    \case
      Print str -> modify (str :)
      Flush -> pure ()
  pure (r, join $ reverse logs)

print :: Print :> es => String -> Eff es ()
print = send . Print

printLn :: Print :> es => String -> Eff es ()
printLn str = print (str <> "\n")

flush :: Print :> es => Eff es ()
flush = send Flush
