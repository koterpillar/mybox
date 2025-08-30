module Mybox.Display.Print (
  Print (..),
  runPrint,
  runPrintPure,
) where

import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Mybox.Prelude

data Print :: Effect where
  Print :: String -> Print m ()

type instance DispatchOf Print = Dynamic

runPrint :: IOE :> es => Eff (Print : es) r -> Eff es r
runPrint = interpret_ $ \case Print str -> liftIO $ putStrLn str

runPrintPure :: Eff (Print : es) r -> Eff es (r, [String])
runPrintPure act = do
  (r, logs) <- reinterpretWith_ (runState @[String] mempty) act $
    \case
      Print str -> modify (str :)
  pure (r, reverse logs)
