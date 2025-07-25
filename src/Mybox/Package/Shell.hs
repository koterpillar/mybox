module Mybox.Package.Shell (ShellPackage (..), mkShellPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Package.Class
import Mybox.Package.Effects
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores

data ShellPackage = ShellPackage
  { shell :: Text
  , root :: Bool
  , post :: [Text]
  }
  deriving (Eq, Show)

mkShellPackage :: Text -> ShellPackage
mkShellPackage shellPath = ShellPackage{shell = shellPath, root = False, post = []}

instance FromJSON ShellPackage where
  parseJSON = withObject "ShellPackage" $ \o -> do
    shellPath <- o .: "shell"
    root <- o .:? "root" .!= False
    post <- parsePost o
    pure ShellPackage{shell = shellPath, ..}

instance ToJSON ShellPackage where
  toJSON p =
    object $ ["shell" .= p.shell, "root" .= p.root] <> postToJSON p

instance HasField "name" ShellPackage Text where
  getField p = "_shell" <> (if p.root then "_root" else "")

shellsFile :: Text
shellsFile = "/etc/shells"

allShells :: Driver :> es => Eff es [Text]
allShells = Text.lines <$> drvReadFile shellsFile

getShellLinux :: Driver :> es => ShellPackage -> Eff es Text
getShellLinux p = do
  username <- if p.root then pure "root" else drvUsername
  passwdEntry <- drvRunOutput $ "getent" :| ["passwd", username]
  let fields = Text.splitOn ":" passwdEntry
  case drop 6 fields of
    (shellPath : _) -> pure shellPath
    [] -> terror "Failed to parse passwd entry"

getShellMacOS :: Driver :> es => ShellPackage -> Eff es Text
getShellMacOS p = do
  home <- if p.root then pure "/root" else drvHome
  result <- drvRunOutput $ "dscl" :| [".", "-read", home, "UserShell"]
  case Text.splitOn ": " result of
    [_, shellPath] -> pure $ Text.strip shellPath
    _ -> terror "Failed to parse dscl output"

shellLocalVersion :: Driver :> es => ShellPackage -> Eff es (Maybe Text)
shellLocalVersion p =
  drvOS >>= \case
    Linux _ -> Just <$> getShellLinux p
    MacOS -> Just <$> getShellMacOS p

shellRemoteVersion :: (Driver :> es, Stores :> es) => ShellPackage -> Eff es Text
shellRemoteVersion p = pure p.shell

shellInstall :: DIST es => ShellPackage -> Eff es ()
shellInstall p = do
  drvOS >>= \case
    Linux Fedora -> queueInstall $ mkSystemPackage "util-linux-user"
    _ -> pure ()

  unlessM (drvIsFile p.shell) $ terror $ p.shell <> " does not exist."
  unlessM (drvIsExecutable p.shell) $ terror $ p.shell <> " is not executable."
  shells <- allShells
  unless (p.shell `elem` shells) $ do
    drvRun $ sudo $ shellRaw $ "echo " <> shellQuote p.shell <> " >> " <> shellQuote shellsFile

  drvRun $ (if p.root then sudo else id) $ "chsh" :| ["-s", p.shell]

instance Package ShellPackage where
  localVersion = shellLocalVersion
  remoteVersion = shellRemoteVersion
  install = installWithPost shellInstall
