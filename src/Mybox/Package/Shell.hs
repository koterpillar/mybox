module Mybox.Package.Shell (ShellPackage (..), mkShellPackage) where

import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Post
import Mybox.Package.Queue
import Mybox.Package.Root
import Mybox.Package.System
import Mybox.Prelude
import Mybox.Stores

data ShellPackage = ShellPackage
  { shell :: Path Abs
  , root :: Bool
  , post :: [Text]
  }
  deriving (Eq, Generic, Show)

mkShellPackage :: Path Abs -> ShellPackage
mkShellPackage shellPath = ShellPackage{shell = shellPath, root = False, post = []}

instance FromJSON ShellPackage where
  parseJSON = withObjectTotal "ShellPackage" $ do
    shellPath <- takeField "shell"
    root <- takeRoot
    post <- takePost
    pure ShellPackage{shell = shellPath, ..}

instance ToJSON ShellPackage where
  toJSON p =
    object $ ["shell" .= p.shell, "root" .= p.root] <> postToJSON p

instance PackageName ShellPackage where
  splitName = genericSplitName' @'["shell"] @'["shell", "root"]

shellsFile :: Path Abs
shellsFile = pRoot </> "etc" </> "shells"

allShells :: Driver :> es => Eff es [Path Abs]
allShells = map mkPath . filter content . Text.lines <$> drvReadFile shellsFile
 where
  content line
    | Text.null line = False
    | Text.isPrefixOf "#" line = False
    | otherwise = True

data PasswdEntry = PasswdEntry
  { username :: Text
  , shellPath :: Text
  }

parsePasswdEntry :: Text -> Maybe PasswdEntry
parsePasswdEntry line = case Text.splitOn ":" line of
  (username : _ : _ : _ : _ : _ : shellPath : _) -> Just PasswdEntry{..}
  _ -> Nothing

getShellLinux :: Driver :> es => Text -> Eff es Text
getShellLinux username = do
  hasGetent <- drvExecutableExists "getent"
  passwdEntry <-
    if hasGetent
      then fmap parsePasswdEntry $ drvRunOutput $ "getent" :| ["passwd", username]
      else do
        passwd <- drvReadFile $ pRoot </> "etc" </> "passwd"
        let entries = catMaybes $ map parsePasswdEntry $ Text.lines passwd
        pure $ find (\e -> e.username == username) entries
  case passwdEntry of
    Just entry -> pure entry.shellPath
    Nothing -> terror $ "Failed to find passwd entry for user: " <> username

getShellMacOS :: Driver :> es => Text -> Eff es Text
getShellMacOS username = do
  result <- drvRunOutput $ "dscl" :| [".", "-read", "/Users/" <> username, "UserShell"]
  case Text.splitOn ": " result of
    [_, shellPath] -> pure $ Text.strip shellPath
    _ -> terror $ "Failed to parse dscl output: " <> result

shellLocalVersion :: Driver :> es => ShellPackage -> Eff es (Maybe Text)
shellLocalVersion p = do
  username <- if p.root then pure "root" else drvUsername
  os <- drvOS
  shellPath <- case os of
    Linux _ -> getShellLinux username
    MacOS -> getShellMacOS username
  pure $ Just shellPath

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
