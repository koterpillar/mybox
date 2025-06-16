module Mybox.SpecBase where

import           Mybox.Driver.Class
import           Mybox.Driver.IO
import           Mybox.Prelude

newtype WD =
  WD (forall r. Eff '[ Driver, IOE] r -> IO r)

withDriver :: (WD -> IO ()) -> IO ()
withDriver ioa =
  runEff $ testDriver $ withSeqEffToIO $ \unlift -> ioa $ WD unlift
