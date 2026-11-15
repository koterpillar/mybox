module Mybox.SelfUpdateSpec where

import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared

import Mybox.Config.CommandLine
import Mybox.Driver
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SelfUpdate qualified as SelfUpdate
import Mybox.Spec.Utils
import Mybox.SpecBase
import Mybox.Tracker

executePure :: Eff (SelfUpdate.Execute : es) r -> Eff es (r, [Args])
executePure act = do
  (r, calls) <- reinterpretWith_ (runState @[Args] mempty) act $
    \case
      SelfUpdate.Execute args -> modify (args :)
  pure (r, reverse calls)

spec :: Spec
spec = do
  withEff (nullTracker . runInstallQueue) $ do
    it "self-updates and re-executes, but only if out of date" $ do
      ((), calls) <- executePure $ SelfUpdate.selfUpdate CommandLine{selfUpdate = True}
      length calls `shouldBe` 1
      let program :| args = requireJust "Expected execute call" $ listToMaybe calls
      local <- drvLocal False
      program `shouldBe` (local </> "bin" </> "mybox").text
      args `shouldBe` []

      ((), calls2) <- executePure $ SelfUpdate.selfUpdate CommandLine{selfUpdate = True}
      calls2 `shouldBe` []
