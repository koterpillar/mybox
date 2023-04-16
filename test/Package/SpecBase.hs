module Package.SpecBase
  ( PackageSpec(..)
  , packageSpec
  , PackageSpecRun(..)
  , psCheckInstalledOutput
  , NonEmpty(..)
  ) where

import           Data.Function        (on)

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           System.IO.Temp
import           System.Process.Typed

import           Test.Hspec

import           Driver
import           Driver.Test

import           Package

data PackageSpecRun =
  PackageSpecRun
    { psrDirectory :: FilePath
    }
  deriving (Show)

withPSR ::
     Package package
  => PackageSpec package
  -> (PackageSpecRun -> IO ())
  -> IO ()
withPSR _ act =
  withSystemTempDirectory "mybox" $ \psrDirectory -> act $ PackageSpecRun {..}

data PackageSpec package =
  PackageSpec
    { psName           :: String
    , psPackage        :: PackageSpecRun -> package
    , psCheckInstalled :: PackageSpecRun -> Expectation
    }

packageSpec :: Package package => PackageSpec package -> Spec
packageSpec ps@PackageSpec {..} =
  around (withPSR ps) $
  describe psName $
  it "is not installed initially" $ \psr ->
    "is installed" `shouldContainText` "implemented"

psCheckInstalledOutput :: NonEmpty Text -> Text -> Expectation
psCheckInstalledOutput cmd expectedOutput = do
  actualOutput <- fmap procDecode $ readProcessStdout_ $ procText cmd
  actualOutput `shouldContainText` expectedOutput

shouldContainText :: HasCallStack => Text -> Text -> Expectation
shouldContainText = shouldContain `on` Text.unpack
