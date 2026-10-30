module Mybox.Package.Hash where

import Mybox.Aeson
import Mybox.Driver
import Mybox.JSONCodec
import Mybox.Package.Name
import Mybox.Prelude
import Mybox.Tracker

newtype PackageHash = PackageHash {hash :: Text} deriving (Eq, Generic, Ord, Show)

instance ToJSON PackageHash where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PackageHash

pkgHash :: (JSONCodec p, PackageName p) => p -> Maybe PackageHash
pkgHash = fmap (PackageHash . jsonEncode . jsonCodec.to) . withoutName

hashFile :: (Driver :> es, PackageName p) => p -> Eff es (Path Abs)
hashFile p =
  fmap
    (\s -> s </> "hashes" </> (pathname p <> ".json"))
    drvMyboxState

localHash :: (Driver :> es, JSONCodec p, PackageName p, Tracker :> es) => p -> Eff es (Maybe PackageHash)
localHash p = do
  hf <- hashFile p
  exists <- drvIsFile hf
  if not exists
    then pure Nothing
    else do
      trkAdd p hf
      jsonDecode @PackageHash "package hash" <$> drvReadFile hf

writeHash :: (Driver :> es, JSONCodec p, PackageName p, Tracker :> es) => p -> Eff es ()
writeHash p = do
  hf <- hashFile p
  case pkgHash p of
    Nothing -> drvRm hf
    Just h -> drvWriteBinaryFile hf $ encode h
  trkAdd p hf
