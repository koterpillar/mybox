module Mybox.Package.ManualVersion.Internal where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Prelude
import Mybox.Release
import Mybox.Tracker

newtype ManualVersion = ManualVersion {version :: Text} deriving (Eq, Generic, Ord, Show)

instance ToJSON ManualVersion where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ManualVersion

versionFile :: (Driver :> es, PackageName p) => p -> Eff es (Path Abs)
versionFile p =
  fmap
    (\s -> s </> "versions" </> (pathname p <> ".json"))
    drvMyboxState

manualVersion :: (Driver :> es, PackageName p, ToJSON p, Tracker :> es) => p -> Eff es (Maybe Text)
manualVersion p = do
  vf <- versionFile p
  exists <- drvIsFile vf
  if not exists
    then pure Nothing
    else do
      trkAdd p vf
      fmap (.version) <$> jsonDecode @ManualVersion "install record" <$> drvReadFile vf

manualVersionInstall :: (App es, Package p) => (p -> Release -> Eff es ()) -> p -> Release -> Eff es ()
manualVersionInstall installAct p release = do
  installAct p release
  vf <- versionFile p
  drvWriteBinaryFile vf $ encode $ ManualVersion{version = release.version}
  trkAdd p vf
