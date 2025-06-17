module Mybox.SpecBase
  ( module Test.Hspec
  , withTestEnv
  , it
  , shouldBe
  , shouldSatisfy
  ) where

import qualified Test.Hspec         as Hspec
import           Test.Hspec         hiding (it, shouldBe, shouldSatisfy)

import           Mybox.Driver.Class
import           Mybox.Driver.IO
import           Mybox.Prelude
import           Mybox.Tracker

newtype RunEff es =
  RunEff (forall r. Eff es r -> IO r)

withTestEnv :: (RunEff '[ Tracker, Driver, IOE] -> IO ()) -> IO ()
withTestEnv ioa =
  runEff
    $ testDriver
    $ nullTracker
    $ withSeqEffToIO
    $ \unlift -> ioa $ RunEff unlift

it :: String -> Eff ef () -> SpecWith (RunEff ef)
it name act = Hspec.it name $ \(RunEff unlift) -> unlift act

shouldBe :: (HasCallStack, Eq a, Show a, IOE :> es) => a -> a -> Eff es ()
shouldBe a b = liftIO $ Hspec.shouldBe a b

shouldSatisfy ::
     (HasCallStack, Show a, IOE :> es) => a -> (a -> Bool) -> Eff es ()
shouldSatisfy a f = liftIO $ Hspec.shouldSatisfy a f
