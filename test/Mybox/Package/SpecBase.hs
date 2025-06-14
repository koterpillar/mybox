module Mybox.Package.SpecBase
  ( PackageSpecArgs(..)
  , PackageSpec
  , ps
  , psCheckInstalled
  , psCheckInstalledCommandOutput
  , packageSpec
  ) where

import           Control.Monad.IO.Class

import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Test.Hspec

import           Mybox.Driver.Class
import           Mybox.Package.Class
import           Mybox.SpecBase

import           System.Random

data PackageSpecArgs = PackageSpecArgs
  { psaRandom    :: StdGen
  , psaDirectory :: Text
  }

mkPSA :: IO PackageSpecArgs
mkPSA = do
  psaRandom <- newStdGen
  psaDirectory <- ("dest-" <>) . Text.pack . show <$> liftIO (randomIO @Int)
  pure $ PackageSpecArgs {..}

psaSpec :: (PackageSpecArgs -> SpecWith d) -> SpecWith d
psaSpec f = runIO mkPSA >>= f

data PackageSpec a = PackageSpec
  { psPackage         :: a
  , psCheckInstalled_ :: forall m. (MonadDriver m, MonadIO m) => m ()
  }

ps :: a -> PackageSpec a
ps p =
  PackageSpec
    { psPackage = p
    , psCheckInstalled_ = liftIO $ expectationFailure "psCheckInstalled not set"
    }

type MPS a = PackageSpec a -> PackageSpec a

psCheckInstalled :: (forall m. (MonadDriver m, MonadIO m) => m ()) -> MPS a
psCheckInstalled f s = s {psCheckInstalled_ = f}

psCheckInstalledCommandOutput :: Args -> Text -> MPS a
psCheckInstalledCommandOutput cmd expectedOutput =
  psCheckInstalled $ do
    actualOutput <- drvRunOutput cmd
    liftIO $ Text.unpack actualOutput `shouldContain` Text.unpack expectedOutput

packageSpec :: Package a => (PackageSpecArgs -> PackageSpec a) -> Spec
packageSpec makePS =
  around withDriver
    $ psaSpec
    $ \psa -> do
        let s = makePS psa
        let p = psPackage s
        describe (Text.unpack $ pkgName p) $ do
          it "has a name" $ \_ -> pkgName p `shouldSatisfy` (not . Text.null)
          it "installs" $ \drv ->
            run drv $ do
              pkgInstall p
              psCheckInstalled_ s
