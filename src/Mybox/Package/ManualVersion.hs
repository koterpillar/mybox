module Mybox.Package.ManualVersion (manualVersion, manualVersionInstall) where

import Data.Aeson qualified as Aeson

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Prelude
import Mybox.Tracker

data InstallRecord = InstallRecord {hash :: Text, version :: Text} deriving (Eq, Generic, Ord, Show)

instance Aeson.ToJSON InstallRecord where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON InstallRecord

versionFile :: PackageName p => p -> Text
versionFile p = pMyboxState </> "versions" </> (p.name <> ".json")

manualVersion :: (Driver :> es, PackageName p, ToJSON p) => p -> Eff es (Maybe Text)
manualVersion p = do
  let vf = versionFile p
  exists <- drvIsFile vf
  if not exists
    then pure Nothing
    else do
      v' <- Aeson.decodeStrictText @InstallRecord <$> drvReadFile (versionFile p)
      pure $ case v' of
        Nothing -> Nothing
        Just v -> if jsonEncode p == v.hash then Just v.version else Nothing

manualVersionInstall ::
  (Driver :> es, Package p, PackageTracker :> es) => (p -> Eff es ()) -> p -> Eff es ()
manualVersionInstall installAct p = do
  v <- remoteVersion p
  installAct p
  drvWriteFile (versionFile p) $ jsonEncode $ InstallRecord{hash = jsonEncode p, version = v}
