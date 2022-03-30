module Installer where

import           Data.Aeson

import           Data.Maybe

import           Data.Text        (Text)
import qualified Data.Text        as Text

import           GHC.Generics

import           System.Directory (findExecutable)

import           Environment
import           GitHub

-- TODO: version check and updates
data Installer =
  Installer
    { iInstall   :: InstallSpec
    , iInstalled :: CheckSpec
    }
  deriving (Show, Generic)

instance FromJSON Installer

iDoInstall :: Installer -> IO ()
iDoInstall = ispDoInstall . iInstall

iIsInstalled :: Installer -> IO Bool
iIsInstalled = cspCheck . iInstalled

data InstallSpec
  = PerOS
      { ispLinux :: InstallSpec
      , ispMacos :: InstallSpec
      }
  | Package
      { ispPackage :: Text
      }
  | GitHub
      { ispRepo :: GitHubRepo
      , ispArtifactFilters :: [Text]
      }
  deriving (Show, Generic)

ispDoInstall :: InstallSpec -> IO ()
ispDoInstall (PerOS l m) =
  currentOS >>=
  (\case
     Linux -> ispDoInstall l
     Macos -> ispDoInstall m)
ispDoInstall (Package _) = error "Not implemented"
ispDoInstall (GitHub _ _) = error "Not implemented"

-- | Installs from a local file.
installArtifact :: Text -> IO ()
installArtifact = error "Not implemented"

newtype CheckSpec =
  Binary
    { cspBinary :: Text
    }
  deriving (Show, Generic)

cspCheck :: CheckSpec -> IO Bool
cspCheck (Binary binary) = isJust <$> findExecutable (Text.unpack binary)

instance FromJSON InstallSpec where
  parseJSON = genericParseJSON specOptions

instance FromJSON CheckSpec where
  parseJSON = genericParseJSON specOptions

specOptions :: Options
specOptions =
  defaultOptions {fieldLabelModifier = drop 3, sumEncoding = UntaggedValue}
