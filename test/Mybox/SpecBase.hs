module Mybox.SpecBase (
  module Test.Hspec,
  withEff,
  withIOEnv,
  withTestEnv,
  EffSpec,
  beforeAll_,
  onlyIf,
  onlyIfOS,
  skipIf,
  it,
  xit,
  shouldBe,
  shouldSatisfy,
  shouldThrow,
  inCI,
  inDocker,
  virtualSystem,
) where

import Control.Exception.Safe (Exception)
import System.Environment
import Test.Hspec hiding (beforeAll_, it, shouldBe, shouldSatisfy, shouldThrow, xit)
import Test.Hspec qualified as Hspec

import Mybox.Driver
import Mybox.Prelude
import Mybox.Stores

newtype RunEff es
  = RunEff (forall r. Eff es r -> IO r)

type EffSpec ef = SpecWith (RunEff ef)

withEff :: IOE :> es => (forall a. Eff es a -> Eff '[IOE] a) -> (RunEff es -> IO ()) -> IO ()
withEff dispatch ioa = runEff $
  dispatch $
    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
      ioa $ RunEff unlift

withIOEnv :: (RunEff '[IOE] -> IO ()) -> IO ()
withIOEnv = withEff id

withTestEnv :: (RunEff '[Driver, Stores, IOE] -> IO ()) -> IO ()
withTestEnv = withEff $ runStores . testDriver

beforeAll_ :: Eff ef () -> EffSpec ef -> EffSpec ef
beforeAll_ act = Hspec.beforeAllWith $ \r@(RunEff unlift) -> unlift act >> pure r

onlyIf :: (forall es. (Driver :> es, IOE :> es) => Eff es Bool) -> SpecWith a -> SpecWith a
onlyIf cond spec =
  runIO (runEff $ testDriver cond)
    >>= \case
      True -> spec
      False -> xdescribe "(skipped)" spec

onlyIfOS :: (OS -> Bool) -> SpecWith a -> SpecWith a
onlyIfOS cond = onlyIf $ cond <$> drvOS

skipIf :: (forall es. (Driver :> es, IOE :> es) => Eff es Bool) -> SpecWith a -> SpecWith a
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

hasEnv :: IOE :> es => String -> Eff es Bool
hasEnv name = not . null . fromMaybe mempty <$> withSeqEffToIO (\_ -> lookupEnv name)

inDocker :: IOE :> es => Eff es Bool
inDocker = hasEnv "DOCKER_IMAGE"

inCI :: IOE :> es => Eff es Bool
inCI = hasEnv "CI"

virtualSystem :: (Driver :> es, IOE :> es) => Eff es Bool
virtualSystem = (||) <$> inDocker <*> inCI
