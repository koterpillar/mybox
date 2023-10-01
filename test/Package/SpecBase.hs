module Package.SpecBase
  ( PackageSpec(..)
  , packageSpec
  , PackageSpecRun(..)
  , psCheckInstalledOutput
  , NonEmpty(..)
  ) where

import           Data.Function  (on)

import           Data.Text      (Text)
import qualified Data.Text      as Text

import           System.IO.Temp

import           Test.Hspec

import           Driver
import           Driver.Test

import           Package

import           State

data PackageSpecRun =
  PackageSpecRun
    { psrDirectory  :: FilePath
    , psrDriver     :: Driver
    , psrConnection :: Connection
    }

withPSR :: PackageSpec package -> (PackageSpecRun -> IO ()) -> IO ()
withPSR _ act =
  withSystemTempDirectory "mybox" $ \dir -> do
    let psrDirectory = dir <> "/destination"
    withLocalTestDriver $ \psrDriver ->
      withSqliteMemoryDB $ \psrConnection -> act $ PackageSpecRun {..}

data PackageSpec package =
  PackageSpec
    { psName           :: String
    , psPackage        :: PackageSpecRun -> package
    , psCheckInstalled :: PackageSpecRun -> Expectation
    }

packageSpec :: Package package => PackageSpec package -> Spec
packageSpec ps@PackageSpec {..} =
  around (withPSR ps) $
  describe psName $ do
    it "installs correctly" $ \psr@PackageSpecRun {..} -> do
      let package = psPackage psr
      pkIsInstalled psrConnection psrDriver package `shouldReturn` False
      pkInstall psrConnection psrDriver package
      pkIsInstalled psrConnection psrDriver package `shouldReturn` True
      psCheckInstalled psr

psCheckInstalledOutput :: PackageSpecRun -> NonEmpty Text -> Text -> Expectation
psCheckInstalledOutput PackageSpecRun {..} cmd expectedOutput = do
  actualOutput <- drvRunOutput cmd psrDriver
  actualOutput `shouldContainText` expectedOutput

shouldContainText :: HasCallStack => Text -> Text -> Expectation
shouldContainText = shouldContain `on` Text.unpack
