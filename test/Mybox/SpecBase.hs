module Mybox.SpecBase (
  module Test.Hspec,
  BaseEff,
  EffSpec,
  Spec,
  effSpec,
  withEff,
  before,
  onlyIf,
  onlyIfOS,
  skipIf,
  expectationFailure,
  it,
  xit,
  shouldBe,
  shouldContain,
  shouldContainText,
  shouldSatisfy,
  shouldThrow,
  inCI,
  inDocker,
  virtualSystem,
  evaluate,
) where

import Data.Text qualified as Text
import Effectful.Exception (evaluate)
import System.Environment
import Test.Hspec hiding (Spec, SpecWith, before, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy, shouldThrow, xit)
import Test.Hspec qualified as Hspec

import Mybox.Driver
import Mybox.Prelude
import Mybox.Stores

newtype RunEff es
  = RunEff (forall r. Eff es r -> IO r)

type EffSpec es = Hspec.SpecWith (RunEff es)

type BaseEff = '[Driver, Stores, Concurrent, IOE]

type Spec = EffSpec BaseEff

effSpec :: IOE :> es => (forall r. Eff es r -> Eff '[IOE] r) -> EffSpec es -> Hspec.Spec
effSpec dispatch = around $ \ioa ->
  runEff $ dispatch $ withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
    ioa $ RunEff unlift

withEff :: (forall a. Eff es' a -> Eff es a) -> EffSpec es' -> EffSpec es
withEff dispatch = mapSubject $ \(RunEff unlift) -> RunEff $ \test -> unlift $ dispatch test

before :: Eff es () -> EffSpec es -> EffSpec es
before act = mapSubject $ \(RunEff unlift) -> RunEff $ \test -> unlift $ act >> test

it :: String -> Eff es () -> EffSpec es
it name act = Hspec.it name $ \(RunEff unlift) -> unlift act

xit :: String -> Eff es () -> EffSpec es
xit name act = Hspec.xit name $ \(RunEff unlift) -> unlift act

shouldBe :: (Eq a, HasCallStack, IOE :> es, Show a) => a -> a -> Eff es ()
shouldBe a b = liftIO $ Hspec.shouldBe a b

shouldSatisfy ::
  (HasCallStack, IOE :> es, Show a) => a -> (a -> Bool) -> Eff es ()
shouldSatisfy a f = liftIO $ Hspec.shouldSatisfy a f

shouldContain :: (Eq a, HasCallStack, IOE :> es, Show a) => [a] -> [a] -> Eff es ()
shouldContain a b = liftIO $ Hspec.shouldContain a b

shouldContainText :: (HasCallStack, IOE :> es) => Text -> Text -> Eff es ()
a `shouldContainText` b = Text.unpack a `shouldContain` Text.unpack b

shouldThrow :: (Exception e, HasCallStack, IOE :> es) => Eff es a -> Selector e -> Eff es ()
shouldThrow act ex = withSeqEffToIO $ \unlift -> Hspec.shouldThrow (unlift act) ex

expectationFailure :: (HasCallStack, IOE :> es) => String -> Eff es ()
expectationFailure = liftIO . Hspec.expectationFailure

onlyIf :: IOE :> es => String -> Eff es Bool -> EffSpec es -> EffSpec es
onlyIf !reason cond =
  mapSubject $ \(RunEff unlift) ->
    RunEff $ \test -> unlift $ do
      x <- cond
      unless x $ liftIO $ Hspec.pendingWith reason
      test

onlyIfOS :: (Driver :> es, IOE :> es) => String -> (OS -> Bool) -> EffSpec es -> EffSpec es
onlyIfOS reason cond = onlyIf reason $ cond <$> drvOS

skipIf :: IOE :> es => String -> Eff es Bool -> EffSpec es -> EffSpec es
skipIf reason cond = onlyIf reason $ fmap not cond

hasEnv :: IOE :> es => String -> Eff es Bool
hasEnv name = not . null . fromMaybe mempty <$> withSeqEffToIO (\_ -> lookupEnv name)

inDocker :: IOE :> es => Eff es Bool
inDocker = hasEnv "DOCKER_IMAGE"

inCI :: IOE :> es => Eff es Bool
inCI = hasEnv "CI"

virtualSystem :: (Driver :> es, IOE :> es) => Eff es Bool
virtualSystem = (||) <$> inDocker <*> inCI
