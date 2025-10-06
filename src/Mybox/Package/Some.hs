module Mybox.Package.Some where

import Mybox.Aeson
import Mybox.Package.BrewRepo
import Mybox.Package.Class
import Mybox.Package.Clone
import Mybox.Package.Daemon
import Mybox.Package.Flatpak
import Mybox.Package.Github
import Mybox.Package.Links
import Mybox.Package.NPM
import Mybox.Package.Pipx
import Mybox.Package.Shell
import Mybox.Package.System
import Mybox.Package.URL
import Mybox.Package.YumRepo
import Mybox.Prelude

newtype SomePackage = SomePackage (forall r. (forall p. Package p => p -> r) -> r)

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
    foldr1
      jsonAlternative
      [ mkSomePackageF (parseJSON @BrewRepo v)
      , mkSomePackageF (parseJSON @ClonePackage v)
      , mkSomePackageF (parseJSON @DaemonPackage v)
      , mkSomePackageF (parseJSON @GithubPackage v)
      , mkSomePackageF (parseJSON @FlatpakPackage v)
      , mkSomePackageF (parseJSON @LinksPackage v)
      , mkSomePackageF (parseJSON @NPMPackage v)
      , mkSomePackageF (parseJSON @PipxPackage v)
      , mkSomePackageF (parseJSON @ShellPackage v)
      , mkSomePackageF (parseJSON @SystemPackage v)
      , mkSomePackageF (parseJSON @URLPackage v)
      , mkSomePackageF (parseJSON @YumRepo v)
      ]

instance ToJSON SomePackage where
  toJSON (SomePackage f) = f toJSON
  toEncoding (SomePackage f) = f toEncoding

instance Eq SomePackage where
  (==) = (==) `on` toJSON

instance Package SomePackage where
  remoteVersion (SomePackage f) = f remoteVersion
  localVersion (SomePackage f) = f localVersion
  install (SomePackage f) = f install
