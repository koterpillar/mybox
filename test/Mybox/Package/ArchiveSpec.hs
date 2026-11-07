module Mybox.Package.ArchiveSpec where

import Data.Text qualified as Text

import Mybox.DesktopFile
import Mybox.Driver
import Mybox.Package.Archive.Internal
import Mybox.Package.Name
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.SpecBase
import Mybox.Tracker

data DummyPackage = DummyPackage {name :: Text, archive :: ArchiveFields}
  deriving (Eq, Generic, Ord, Show)

instance PackageName DummyPackage where
  splitName = genericSplitName

instance ArchivePackage DummyPackage where
  archiveUrl _ = pure "dummy"

spec :: Spec
spec = do
  describe "findBinary" $ do
    let pkg = DummyPackage{name = "name", archive = emptyArchiveFields}
    it "finds a binary in specified paths" $ do
      dir <- aDirectory pkg
      let bin = dir </> "bin" </> "test-binary"
      drvWriteFile bin ""
      drvMakeExecutable bin
      findBinary pkg "test-binary" >>= (`shouldBe` bin)
    it "errors when binary does not exist" $ do
      findBinary pkg "test-binary" `shouldThrow` anyErrorCall
  describe "parseDesktopFile" $ do
    it "parses the main group, ignoring comments and desktop actions" $
      parseDesktopFile
        ( Text.unlines
            [ "[Desktop Action new-window]"
            , "Name=New Window"
            , "Exec=test --new-window"
            , "[Desktop Entry]"
            , "# a comment"
            , "Name=Test"
            , "Exec=test --url=https://example.com"
            , "Icon=test"
            , "[Desktop Action new-tab]"
            , "Exec=test --new-tab"
            ]
        )
        `shouldBe` DesktopFile{name = "Test", exec = "test --url=https://example.com", icon = Just "test"}
    it "defaults missing keys" $
      parseDesktopFile "[Desktop Entry]\nName=Test\n"
        `shouldBe` DesktopFile{name = "Test", exec = "", icon = Nothing}
  describe "renderDesktopFile" $ do
    it "renders a minimal entry" $
      renderDesktopFile DesktopFile{name = "App", exec = "/usr/local/bin/app", icon = Just "app"}
        `shouldBe` Text.unlines
          [ "[Desktop Entry]"
          , "Type=Application"
          , "Name=App"
          , "Exec=/usr/local/bin/app"
          , "Icon=app"
          , "Terminal=false"
          ]
    it "omits the icon when there is none" $
      renderDesktopFile DesktopFile{name = "App", exec = "app", icon = Nothing}
        `shouldBe` Text.unlines
          [ "[Desktop Entry]"
          , "Type=Application"
          , "Name=App"
          , "Exec=app"
          , "Terminal=false"
          ]
  withEff (nullTracker . runInstallQueue) $
    onlyIfOS "apps are only installed on Linux" (\case Linux _ -> True; _ -> False) $
      describe "installApp" $ do
        let pkg = DummyPackage{name = "name", archive = emptyArchiveFields}
        it "links a desktop file shipped in the archive" $ do
          dir <- aDirectory pkg
          let shipped = dir </> "share" </> "applications" </> "app.desktop"
          drvWriteFile shipped "[Desktop Entry]\nName=Shipped\nExec=shipped\nIcon=app\n"
          installApp pkg "app"
          local <- drvLocal False
          desktopFile <-
            parseDesktopFile
              <$> drvReadFile (local </> "share" </> "applications" </> "app.desktop")
          desktopFile `shouldBe` DesktopFile{name = "Shipped", exec = "shipped", icon = Just "app"}
        it "creates a minimal desktop file with the archive's icon" $ do
          dir <- aDirectory pkg
          drvWriteFile (dir </> "app.svg") "<svg/>"
          installApp pkg "app"
          local <- drvLocal False
          desktopFile <-
            parseDesktopFile
              <$> drvReadFile (local </> "share" </> "applications" </> "app.desktop")
          desktopFile `shouldBe` DesktopFile{name = "app", exec = (local </> "bin" </> "app").text, icon = Just "app"}
          let installedIcon = local </> "share" </> "icons" </> "hicolor" </> "scalable" </> "apps" </> "app.svg"
          unlessM (drvIsFile installedIcon) $ expectationFailure ("Icon not installed: " <> Text.unpack installedIcon.text)
        it "creates a minimal desktop file without an icon" $ do
          dir <- aDirectory pkg
          drvWriteFile (dir </> "app") "binary"
          installApp pkg "app"
          local <- drvLocal False
          desktopFile <-
            parseDesktopFile
              <$> drvReadFile (local </> "share" </> "applications" </> "app.desktop")
          desktopFile `shouldBe` DesktopFile{name = "app", exec = (local </> "bin" </> "app").text, icon = Nothing}
  describe "withExtensions" $ do
    let extensions = ["txt", "md"]
    it "returns the same extension if already given" $
      withExtensions extensions "file.txt" `shouldBe` ["file.txt"]
    it "adds all extensions if not given" $
      withExtensions extensions "file" `shouldBe` ["file.txt", "file.md"]
    it "errors on multiple extensions" $ do
      evaluate (withExtensions extensions "file.txt.md") `shouldThrow` anyErrorCall
  describe "iconPath" $ do
    it "returns correct path for SVG" $
      iconPath (mkPath @AnyAnchor "icon.svg") `shouldBe` mkPath "hicolor/scalable/apps/icon.svg"
    it "returns correct path for PNG" $ do
      iconPath (mkPath @AnyAnchor "icon.png") `shouldBe` mkPath "hicolor/16x16/apps/icon.png"
    it "returns path with resolution for PNG" $ do
      iconPath (mkPath @AnyAnchor "somewhere/32x32/icon.png") `shouldBe` mkPath "hicolor/32x32/apps/icon.png"
    it "errors on unsupported extension" $ do
      evaluate ((iconPath $ mkPath @AnyAnchor "icon.jpg").text)
        `shouldThrow` errorCall "Unexpected icon extension: jpg from icon.jpg"
