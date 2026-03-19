module Mybox.Package.BrewRepoSpec where

import Mybox.Package.BrewRepo
import Mybox.Package.BrewRepo.Internal (tapName)
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Package.SpecBase
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

spec :: Spec
spec = do
  describe "tapName" $ do
    it "returns GitHub shortcut as-is for owner/repo format" $
      tapName (mkBrewRepo "owner/repo") `shouldBe` "owner/repo"
    it "strips https:// protocol from full URL" $
      tapName (mkBrewRepo "https://github.com/owner/repo.git") `shouldBe` "github.com_owner/repo"
    it "strips git@ username and replaces : with _" $
      tapName (mkBrewRepo "git@github.com:owner/repo.git") `shouldBe` "github.com_owner/repo"
    it "strips ssh:// protocol, username, and replaces : with _" $
      tapName (mkBrewRepo "ssh://git@gitlab.com:owner/repo.git") `shouldBe` "gitlab.com_owner/repo"
    it "leaves exactly two segments of the path, even if there are more" $
      tapName (mkBrewRepo "https://example.com/path/to/repo.git") `shouldBe` "example.com_path_to/repo"
    it "makes up two segments if there is only one" $
      tapName (mkBrewRepo "https://example.com/") `shouldBe` "example.com/_"
    it "errors if there are no path segments" $
      evaluate (tapName (mkBrewRepo "https://")) `shouldThrow` anyException
  metaSpec
    @BrewRepo
    [ (Nothing, "{\"brew_tap\": \"easysoft/tap\"}")
    ]
  onlyIf "Homebrew repository tests require virtual system (Docker or CI)" virtualSystem $ do
    skipGenericLinux "Default installer is unavailable on generic Linux" $ do
      withEff (nullTracker . runInstallQueue) $ do
        it "reports not installed when not tapped" $ do
          enableSudo
          localVersion (mkBrewRepo "denji/nginx") >>= (`shouldBe` Nothing)
      packageSpec $
        ps (mkBrewRepo "easysoft/tap")
          & preinstallEnableSudo
          & checkInstalledCommandOutput ("brew" :| ["info", "qcadmin"]) "qcadmin is an open-source lightweight cli tool for managing quickon"
