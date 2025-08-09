module Mybox.Package.ManualVersion (manualVersion, manualVersionInstall) where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Effects
import Mybox.Prelude

data InstallRecord = InstallRecord {hash :: Text, version :: Text} deriving (Eq, Generic, Ord, Show)

instance ToJSON InstallRecord where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON InstallRecord

versionFile :: (Driver :> es, PackageName p) => p -> Eff es (Path Abs)
versionFile p =
  fmap
    (\s -> s </> "versions" </> (pathname p <> ".json"))
    drvMyboxState

manualVersion :: (Driver :> es, PackageName p, ToJSON p) => p -> Eff es (Maybe Text)
manualVersion p = do
  vf <- versionFile p
  exists <- drvIsFile vf
  if not exists
    then pure Nothing
    else do
      v' <- jsonDecode @InstallRecord "install record" <$> drvReadFile vf
      pure $ case v' of
        Nothing -> Nothing
        Just v -> if jsonEncode p == v.hash then Just v.version else Nothing

manualVersionInstall :: (DIST es, Package p) => (p -> Eff es ()) -> p -> Eff es ()
manualVersionInstall installAct p = do
  v <- remoteVersion p
  installAct p
  vf <- versionFile p
  drvWriteBinaryFile vf $ encode $ InstallRecord{hash = jsonEncode p, version = v}
