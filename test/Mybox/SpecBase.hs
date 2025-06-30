module Mybox.SpecBase
  ( module Test.Hspec,
    withIOEnv,
    withTestEnv,
    it,
    shouldBe,
    shouldSatisfy,
    shouldThrow,
  )
where

import Control.Exception.Safe (Exception)

import Mybox.Driver.Class
import Mybox.Driver.IO
import Mybox.Prelude
import Test.Hspec hiding (it, shouldBe, shouldSatisfy, shouldThrow)
import Test.Hspec qualified as Hspec

newtype RunEff es
  = RunEff (forall r. Eff es r -> IO r)

withIOEnv :: (RunEff '[IOE] -> IO ()) -> IO ()
withIOEnv ioa = runEff $ withSeqEffToIO $ \unlift -> ioa $ RunEff unlift

withTestEnv :: (RunEff '[Driver, IOE] -> IO ()) -> IO ()
withTestEnv ioa =
  runEff $ testDriver $ withSeqEffToIO $ \unlift -> ioa $ RunEff unlift

it :: String -> Eff ef () -> SpecWith (RunEff ef)
it name act = Hspec.it name $ \(RunEff unlift) -> unlift act

shouldBe :: (HasCallStack, Eq a, Show a, IOE :> es) => a -> a -> Eff es ()
shouldBe a b = liftIO $ Hspec.shouldBe a b

shouldSatisfy ::
  (HasCallStack, Show a, IOE :> es) => a -> (a -> Bool) -> Eff es ()
shouldSatisfy a f = liftIO $ Hspec.shouldSatisfy a f

shouldThrow :: (HasCallStack, Exception e, IOE :> es) => Eff es a -> Selector e -> Eff es ()
shouldThrow act ex = withSeqEffToIO $ \unlift -> Hspec.shouldThrow (unlift act) ex
