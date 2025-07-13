module Mybox.Package.Some where

import Control.Applicative ((<|>))

import Mybox.Aeson
import Mybox.Package.BrewRepo
import Mybox.Package.Class
import Mybox.Package.Clone
import Mybox.Package.NPM
import Mybox.Package.Pipx
import Mybox.Package.System
import Mybox.Package.YumRepo
import Mybox.Prelude

newtype SomePackage = SomePackage {package :: forall r. (forall p. Package p => p -> r) -> r}

mkSomePackage :: Package p => p -> SomePackage
mkSomePackage p = SomePackage $ \f -> f p

mkSomePackageF :: (Functor f, Package p) => f p -> f SomePackage
mkSomePackageF = fmap mkSomePackage

instance HasField "name" SomePackage Text where
  getField (SomePackage f) = f (.name)

instance Show SomePackage where
  show (SomePackage f) = "SomePackage " <> f show

instance FromJSON SomePackage where
  parseJSON v =
    mkSomePackageF (parseJSON @BrewRepo v)
      <|> mkSomePackageF (parseJSON @ClonePackage v)
      <|> mkSomePackageF (parseJSON @NPMPackage v)
      <|> mkSomePackageF (parseJSON @PipxPackage v)
      <|> mkSomePackageF (parseJSON @SystemPackage v)
      <|> mkSomePackageF (parseJSON @YumRepo v)

instance ToJSON SomePackage where
  toJSON (SomePackage f) = f toJSON
  toEncoding (SomePackage f) = f toEncoding

instance Package SomePackage where
  remoteVersion (SomePackage f) = f remoteVersion
  localVersion (SomePackage f) = f localVersion
  install (SomePackage f) = f install
