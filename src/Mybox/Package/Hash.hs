module Mybox.Package.Hash where

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Name
import Mybox.Prelude
import Mybox.Tracker

newtype PackageHash = PackageHash {hash :: Text} deriving (Eq, Generic, Ord, Show)

instance ToJSON PackageHash where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PackageHash

pkgHash :: ToJSON p => p -> PackageHash
pkgHash = PackageHash . jsonEncode

hashFile :: (Driver :> es, PackageName p) => p -> Eff es (Path Abs)
hashFile p =
  fmap
    (\s -> s </> "hashes" </> (pathname p <> ".json"))
    drvMyboxState

localHash :: (Driver :> es, PackageName p, ToJSON p, Tracker :> es) => p -> Eff es (Maybe PackageHash)
localHash p = do
  hf <- hashFile p
  exists <- drvIsFile hf
  if not exists
    then pure Nothing
    else do
      trkAdd p hf
      jsonDecode @PackageHash "package hash" <$> drvReadFile hf

writeHash :: (Driver :> es, PackageName p, ToJSON p, Tracker :> es) => p -> Eff es ()
writeHash p = do
  hf <- hashFile p
  drvWriteBinaryFile hf $ encode $ pkgHash p
  trkAdd p hf
