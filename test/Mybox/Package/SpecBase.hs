module Mybox.Package.SpecBase
  ( PackageSpecArgs(..)
  , PackageSpec
  , ps
  , psName
  , psCheckInstalled
  , psCheckInstalledCommandOutput
  , psPreinstall
  , packageSpec
  ) where

import           Control.Monad.IO.Class

import           Data.Maybe

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
  , psName_           :: Maybe Text
  , psCheckInstalled_ :: forall m. (MonadDriver m, MonadIO m) => m ()
  , psPreinstall_     :: forall m. MonadDriver m => m ()
  }

ps :: a -> PackageSpec a
ps p =
  PackageSpec
    { psPackage = p
    , psName_ = Nothing
    , psCheckInstalled_ = liftIO $ expectationFailure "psCheckInstalled not set"
    , psPreinstall_ = pure ()
    }

type MPS a = PackageSpec a -> PackageSpec a

psName :: Text -> MPS a
psName n s = s {psName_ = Just n}

psCheckInstalled :: (forall m. (MonadDriver m, MonadIO m) => m ()) -> MPS a
psCheckInstalled f s = s {psCheckInstalled_ = f}

psCheckInstalledCommandOutput :: Args -> Text -> MPS a
psCheckInstalledCommandOutput cmd expectedOutput =
  psCheckInstalled $ do
    actualOutput <- drvRunOutput cmd
    liftIO $ Text.unpack actualOutput `shouldContain` Text.unpack expectedOutput

psPreinstall :: (forall m. MonadDriver m => m ()) -> MPS a
psPreinstall f s = s {psPreinstall_ = psPreinstall_ s >> f}

packageSpec :: Package a => (PackageSpecArgs -> PackageSpec a) -> Spec
packageSpec makePS =
  around withDriver
    $ psaSpec
    $ \psa -> do
        let s = makePS psa
        let p = psPackage s
        describe (Text.unpack $ fromMaybe (pkgName p) (psName_ s)) $ do
          it "has a name" $ \_ -> pkgName p `shouldSatisfy` (not . Text.null)
          it "installs" $ \drv ->
            run drv $ do
              psPreinstall_ s
              pkgInstall p
              psCheckInstalled_ s
