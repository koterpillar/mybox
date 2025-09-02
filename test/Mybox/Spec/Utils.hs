module Mybox.Spec.Utils (
  module Mybox.Spec.Uncovered.Utils,
  module Mybox.Spec.Utils,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import System.Random

import Mybox.Spec.Uncovered.Utils

randomText :: IOE :> es => Text -> Eff es Text
randomText prefix = ((prefix <> "-") <>) <$> Text.pack . show <$> liftIO (randomIO @Word)
