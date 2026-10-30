module Mybox.Package.Some where

import Mybox.Aeson
import Mybox.JSONEx
import Mybox.Package.BrewRepo
import Mybox.Package.Class
import Mybox.Package.Clone
import Mybox.Package.Daemon
import Mybox.Package.Forge
import Mybox.Package.Links
import Mybox.Package.NPM
import Mybox.Package.Python
import Mybox.Package.Shell
import Mybox.Package.Skill
import Mybox.Package.System
import Mybox.Package.URL
import Mybox.Package.YumRepo
import Mybox.Prelude

newtype SomePackage = SomePackage (forall r. (forall p. Package p => p -> r) -> r)

mkSomePackage :: Package p => p -> SomePackage
mkSomePackage p = SomePackage $ \f -> f p

mkSomePackageF :: (Functor f, Package p) => f p -> f SomePackage
mkSomePackageF = fmap mkSomePackage

instance Show SomePackage where
  show (SomePackage f) = "SomePackage " <> f show

-- | A package type, for enumerating all of them.
data PackageType = forall p. Package p => PackageType (Proxy p)

-- | All package types, in the order they are tried when parsing.
packageTypes :: [PackageType]
packageTypes =
  [ PackageType $ Proxy @BrewRepo
  , PackageType $ Proxy @ClonePackage
  , PackageType $ Proxy @DaemonPackage
  , PackageType $ Proxy @LinksPackage
  , PackageType $ Proxy @NPMPackage
  , PackageType $ Proxy @PythonPackage
  , PackageType $ Proxy @ForgePackage
  , PackageType $ Proxy @ShellPackage
  , PackageType $ Proxy @SkillPackage
  , PackageType $ Proxy @SystemPackage
  , PackageType $ Proxy @URLPackage
  , PackageType $ Proxy @YumRepo
  ]

instance JSONEx SomePackage where
  jsonEx =
    ValueCodec
      { toValue = \(SomePackage f) -> f jsonEx.toValue
      , parseValue = \v ->
          foldr1
            jsonAlternative
            [mkSomePackageF $ (jsonEx @p).parseValue v | PackageType (_ :: Proxy p) <- packageTypes]
      , valueDoc = DocOneOf [(jsonEx @p).valueDoc | PackageType (_ :: Proxy p) <- packageTypes]
      }

instance FromJSON SomePackage where
  parseJSON = jsonEx.parseValue

instance ToJSON SomePackage where
  toJSON = jsonEx.toValue

instance Eq SomePackage where
  (==) = (==) `on` toJSON

instance PackageName SomePackage where
  -- Existential `p` cannot escape the argument scope, pattern match on a
  -- boolean instead.
  splitName (SomePackage f) = (f (fst . splitName), rest)
   where
    rest = case f (isJust . snd . splitName) of
      False -> Nothing
      True -> Just $ SomePackage $ \g -> f $ g . fromJust . snd . splitName

instance Package SomePackage where
  remoteVersion (SomePackage f) = f remoteVersion
  localVersion (SomePackage f) = f localVersion
  install (SomePackage f) = f install
