module Mybox.DisplaySpec where

import Mybox.Display
import Mybox.Display.SpecUtils
import Mybox.SpecBase

spec :: Spec
spec = do
  describe "runDisplay" $
    it "selects simple display" $ do
      ((), output) <- writeHandle $ \h -> runDisplay h $ displayLogText "hello"
      output `shouldBe` "hello\n"
