module Mybox.Package.ManualVersion.Internal where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Prelude
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

manualVersionInstall :: (App es, Package p) => (p -> Eff es ()) -> p -> Eff es ()
manualVersionInstall installAct p = do
  v <- remoteVersion p
  installAct p
  vf <- versionFile p
  drvWriteBinaryFile vf $ encode $ ManualVersion{version = v}
  trkAdd p vf
