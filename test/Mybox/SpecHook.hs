module Mybox.SpecHook where

import Control.Concurrent.MVar
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Ord (Down (..))
import Data.Text qualified as Text
import Test.Hspec qualified as Hspec

import Mybox.Display.None
import Mybox.Driver
import Mybox.Driver.Stats
import Mybox.Driver.Test
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Stores

hook :: Spec -> Hspec.Spec
hook spec = do
  globalStats <- runIO $ newMVar Map.empty
  afterAll_ (takeMVar globalStats >>= printStats 20) $ effSpec (dispatch globalStats) spec

dispatch :: MVar (Map Args Int) -> Eff BaseEff r -> Eff '[IOE] r
dispatch globalStats act =
  runConcurrent $
    noDisplay $
      runStores $ do
        (stats, r) <- testDriver $ driverStats act
        liftIO $ modifyMVar_ globalStats $ pure . Map.unionWith (+) stats
        pure r

printStats :: Int -> Map Args Int -> IO ()
printStats n stats = do
  let top = take n $ sortOn (Down . snd) $ Map.toList stats
  putStrLn $ "Top " <> show n <> " commands:"
  forM_ top $ \(arg, count) ->
    putStrLn $ Text.unpack (shellJoin arg) <> ": " <> show count
