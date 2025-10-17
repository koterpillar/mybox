module Mybox.Spec.Utils (
  module Mybox.Spec.Uncovered.Utils,
  module Mybox.Spec.Utils,
) where

import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent (forkIO)
import Effectful.Concurrent.QSemN
import Effectful.Exception
import System.Random

import Mybox.Prelude
import Mybox.Spec.Uncovered.Utils

randomText :: IOE :> es => Text -> Eff es Text
randomText prefix = (("mybox-" <> prefix <> "-") <>) <$> Text.pack . show <$> liftIO (randomIO @Word)

errorCallContains :: [Text] -> ErrorCall -> Bool
errorCallContains expected (ErrorCall msg) =
  all (`Text.isInfixOf` Text.pack msg) expected

concurrently :: Concurrent :> es => Int -> (Int -> Eff es ()) -> Eff es ()
concurrently times act = do
  sem <- newQSemN 0
  void $ forM [1 .. times] $ \i -> forkIO $ do
    act i
    signalQSemN sem 1
  waitQSemN sem times

concurrently_ :: Concurrent :> es => Int -> Eff es () -> Eff es ()
concurrently_ times = concurrently times . const
