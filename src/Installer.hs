{-# LANGUAGE DeriveGeneric #-}

module Installer where

import           Data.Aeson
import           Data.Text    (Text)

import           GHC.Generics

import           Environment

data Installer =
  Installer
    { iInstall   :: InstallSpec
    , iInstalled :: CheckSpec
    }
  deriving (Show, Generic)

instance FromJSON Installer

-- TODO: version check and updates
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
  | Github
      { ispOrg  :: Text
      , ispRepo :: Text
      }
  deriving (Show, Generic)

ispDoInstall :: InstallSpec -> IO ()
ispDoInstall (PerOS l m) =
  currentOS >>=
  (\case
     Linux -> ispDoInstall l
     Macos -> ispDoInstall m)
ispDoInstall (Package _) = _
ispDoInstall (Github _ _) = _

newtype CheckSpec =
  Binary
    { cspBinary :: Text
    }
  deriving (Show, Generic)

cspCheck :: CheckSpec -> IO Bool
cspCheck = _

instance FromJSON InstallSpec where
  parseJSON = genericParseJSON specOptions

instance FromJSON CheckSpec where
  parseJSON = genericParseJSON specOptions

specOptions :: Options
specOptions =
  defaultOptions {fieldLabelModifier = drop 3, sumEncoding = UntaggedValue}
