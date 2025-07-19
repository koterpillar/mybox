module Mybox.Package.ManualVersion (manualVersion, manualVersionInstall) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Queue
import Mybox.Prelude
import Mybox.Stores
import Mybox.Tracker

data InstallRecord = InstallRecord {hash :: Text, version :: Text} deriving (Eq, Generic, Ord, Show)

instance ToJSON InstallRecord where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON InstallRecord

versionFile :: PackageName p => p -> Text
versionFile p = pMyboxState </> "versions" </> (p.name <> ".json")

manualVersion :: (Driver :> es, PackageName p, ToJSON p) => p -> Eff es (Maybe Text)
manualVersion p = do
  let vf = versionFile p
  exists <- drvIsFile vf
  if not exists
    then pure Nothing
    else do
      v' <- jsonDecode @InstallRecord "install record" <$> drvReadFile (versionFile p)
      pure $ case v' of
        Nothing -> Nothing
        Just v -> if jsonEncode p == v.hash then Just v.version else Nothing

manualVersionInstall ::
  (Driver :> es, InstallQueue :> es, Package p, Stores :> es, TrackerSession :> es) => (p -> Eff es ()) -> p -> Eff es ()
manualVersionInstall installAct p = do
  v <- remoteVersion p
  installAct p
  drvWriteBinaryFile (versionFile p) $ encode $ InstallRecord{hash = jsonEncode p, version = v}
