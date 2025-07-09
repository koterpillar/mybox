module Mybox.SpecBase (
  module Test.Hspec,
  withIOEnv,
  withTestEnv,
  EffSpec,
  onlyIf,
  skipIf,
  it,
  xit,
  shouldBe,
  shouldSatisfy,
  shouldThrow,
) where

import Control.Exception.Safe (Exception)
import Test.Hspec hiding (it, shouldBe, shouldSatisfy, shouldThrow, xit)
import Test.Hspec qualified as Hspec

import Mybox.Driver.Class
import Mybox.Driver.IO
import Mybox.Prelude
import Mybox.Stores

newtype RunEff es
  = RunEff (forall r. Eff es r -> IO r)

type EffSpec ef = SpecWith (RunEff ef)

withIOEnv :: (RunEff '[IOE] -> IO ()) -> IO ()
withIOEnv ioa = runEff $ withSeqEffToIO $ \unlift -> ioa $ RunEff unlift

withTestEnv :: (RunEff '[Driver, Stores, IOE] -> IO ()) -> IO ()
withTestEnv ioa =
  runEff $ runStores $ testDriver $ withSeqEffToIO $ \unlift -> ioa $ RunEff unlift

onlyIf :: (forall es. Driver :> es => Eff es Bool) -> SpecWith a -> SpecWith a
onlyIf cond spec =
  runIO (runEff $ testDriver cond)
    >>= \case
      True -> spec
      False -> xdescribe "(skipped)" spec

skipIf :: (forall es. Driver :> es => Eff es Bool) -> SpecWith a -> SpecWith a
skipIf cond = onlyIf $ fmap not cond

it :: String -> Eff ef () -> EffSpec ef
it name act = Hspec.it name $ \(RunEff unlift) -> unlift act

xit :: String -> Eff ef () -> EffSpec ef
xit name act = Hspec.xit name $ \(RunEff unlift) -> unlift act

shouldBe :: (Eq a, HasCallStack, IOE :> es, Show a) => a -> a -> Eff es ()
shouldBe a b = liftIO $ Hspec.shouldBe a b

shouldSatisfy ::
  (HasCallStack, IOE :> es, Show a) => a -> (a -> Bool) -> Eff es ()
shouldSatisfy a f = liftIO $ Hspec.shouldSatisfy a f

shouldThrow :: (Exception e, HasCallStack, IOE :> es) => Eff es a -> Selector e -> Eff es ()
shouldThrow act ex = withSeqEffToIO $ \unlift -> Hspec.shouldThrow (unlift act) ex
