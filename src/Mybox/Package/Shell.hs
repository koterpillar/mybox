module Mybox.Package.Shell (ShellPackage (..), mkShellPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores

data ShellPackage = ShellPackage
  { shell :: Path Abs
  , root :: Bool
  , post :: [Text]
  }
  deriving (Eq, Show)

mkShellPackage :: Path Abs -> ShellPackage
mkShellPackage shellPath = ShellPackage{shell = shellPath, root = False, post = []}

instance FromJSON ShellPackage where
  parseJSON = withObjectTotal "ShellPackage" $ do
    shellPath <- takeField "shell"
    root <- fromMaybe False <$> takeFieldMaybe "root"
    post <- takePost
    pure ShellPackage{shell = shellPath, ..}

instance ToJSON ShellPackage where
  toJSON p =
    object $ ["shell" .= p.shell, "root" .= p.root] <> postToJSON p

instance HasField "name" ShellPackage Text where
  getField p = "_shell" <> (if p.root then "_root" else "")

shellsFile :: Path Abs
shellsFile = pRoot </> "etc" </> "shells"

allShells :: Driver :> es => Eff es [Path Abs]
allShells = map mkPath . filter content . Text.lines <$> drvReadFile shellsFile
 where
  content line
    | Text.null line = False
    | Text.isPrefixOf "#" line = False
    | otherwise = True

getShellLinux :: Driver :> es => ShellPackage -> Eff es Text
getShellLinux p = do
  username <- if p.root then pure "root" else drvUsername
  passwdEntry <- drvRunOutput $ "getent" :| ["passwd", username]
  let fields = Text.splitOn ":" passwdEntry
  case drop 6 fields of
    (shellPath : _) -> pure shellPath
    [] -> terror $ "Failed to parse passwd entry: " <> passwdEntry

getShellMacOS :: Driver :> es => ShellPackage -> Eff es Text
getShellMacOS p = do
  username <- if p.root then pure "root" else drvUsername
  result <- drvRunOutput $ "dscl" :| [".", "-read", "/Users/" <> username, "UserShell"]
  case Text.splitOn ": " result of
    [_, shellPath] -> pure $ Text.strip shellPath
    _ -> terror $ "Failed to parse dscl output: " <> result

shellLocalVersion :: Driver :> es => ShellPackage -> Eff es (Maybe Text)
shellLocalVersion p =
  drvOS >>= \case
    Linux _ -> Just <$> getShellLinux p
    MacOS -> Just <$> getShellMacOS p

shellRemoteVersion :: (Driver :> es, Stores :> es) => ShellPackage -> Eff es Text
shellRemoteVersion p = pure p.shell.text

shellInstall :: App es => ShellPackage -> Eff es ()
shellInstall p = do
  drvOS >>= \case
    Linux Fedora -> queueInstall $ mkSystemPackage "util-linux-user"
    _ -> pure ()

  unlessM (drvIsFile p.shell) $ terror $ p.shell.text <> " does not exist."
  unlessM (drvIsExecutable p.shell) $ terror $ p.shell.text <> " is not executable."
  shells <- allShells
  sudo' <- mkSudo
  unless (p.shell `elem` shells) $ do
    drvRun $ sudo' $ shellRaw $ "echo " <> shellQuote p.shell.text <> " >> " <> shellQuote shellsFile.text

  drvRun $ (if p.root then sudo' else id) $ "chsh" :| ["-s", p.shell.text]

instance Package ShellPackage where
  localVersion = shellLocalVersion
  remoteVersion = shellRemoteVersion
  install = installWithPost shellInstall
