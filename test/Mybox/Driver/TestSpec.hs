module Mybox.Driver.TestSpec where

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.Platform
import Mybox.Prelude
import Mybox.SpecBase

spec :: Spec
spec = do
  onlyIf "Pre-downloads are only done in Docker" inDocker $ do
    onlyIfOS
      "Apt is only available on Debian-based systems"
      (\case Linux (Debian _) -> True; _ -> False)
      $ do
        it "pre-downloads apt packages" $ do
          drvRunOutput (sudo $ "apt" :| ["install", "-y", "git"])
            -- "Need to get 0 B" or "Download size: 0 B"
            >>= (`shouldContainText` " 0 B")
