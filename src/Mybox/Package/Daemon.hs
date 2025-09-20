module Mybox.Package.Daemon (
  DaemonPackage (..),
  mkDaemonPackage,
  daemonName,
  daemonDescription,
) where

import Data.Char (isAlphaNum)
import Data.Text qualified as Text

import Mybox.Aeson
import Mybox.Driver
import Mybox.Effects
import Mybox.Package.Class
import Mybox.Package.Post
import Mybox.Prelude
import Mybox.Tracker

data DaemonPackage = DaemonPackage
  { daemon :: NonEmpty Text
  , nameOverride :: Maybe Text
  , post :: [Text]
  }
  deriving (Eq, Show)

mkDaemonPackage :: NonEmpty Text -> DaemonPackage
mkDaemonPackage daemon = DaemonPackage{daemon, nameOverride = Nothing, post = []}

instance FromJSON DaemonPackage where
  parseJSON = withObjectTotal "DaemonPackage" $ do
    daemon <- takeCollapsedNEList "daemon"
    nameOverride <- takeFieldMaybe "name"
    post <- takePost
    pure DaemonPackage{..}

instance ToJSON DaemonPackage where
  toJSON p =
    object $
      [ "daemon" .= p.daemon
      ]
        <> maybe [] (\n -> ["name" .= n]) p.nameOverride
        <> postToJSON p

instance HasField "name" DaemonPackage Text where
  getField p = fromMaybe (cmd p) p.nameOverride

cmd :: DaemonPackage -> Text
cmd p = shellJoin p.daemon

daemonName :: DaemonPackage -> Text
daemonName p = "com.koterpillar.mybox." <> clean (cmd p)
 where
  clean = Text.map cleanChar . Text.replace "'" "" . Text.replace "\"" ""
  cleanChar c
    | isAlphaNum c = c
    | otherwise = '_'

daemonDescription :: DaemonPackage -> Text
daemonDescription p = "Mybox: " <> p.name

serviceFileLinux :: DaemonPackage -> Text
serviceFileLinux p =
  Text.unlines
    [ "[Unit]"
    , "Description=" <> daemonDescription p
    , ""
    , "[Service]"
    , "Restart=always"
    , "ExecStart=" <> Text.unwords (toList $ shell p.daemon)
    , ""
    , "[Install]"
    , "WantedBy=default.target"
    , ""
    ]

serviceFileMacOS :: DaemonPackage -> Text
serviceFileMacOS p =
  Text.unlines $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
    , "<plist version=\"1.0\">"
    , "<dict>"
    , "    <key>Label</key>"
    , mkString $ daemonDescription p
    , "    <key>RunAtLoad</key>"
    , "    <true/>"
    , "    <key>KeepAlive</key>"
    , "    <true/>"
    , "    <key>ProgramArguments</key>"
    , "    <array>"
    , "    <string>/usr/bin/env</string>"
    , "    <string>-SPATH=${HOME}/.local/bin:${PATH}</string>"
    ]
      <> map mkString (toList $ shell p.daemon)
      <> [ "    </array>"
         , "</dict>"
         , "</plist>"
         ]
 where
  mkString arg = "<string>" <> arg <> "</string>"

serviceFileContent :: Driver :> es => DaemonPackage -> Eff es Text
serviceFileContent p = do
  os <- drvOS
  pure $ case os of
    Linux _ -> serviceFileLinux p
    MacOS -> serviceFileMacOS p

serviceFilePath :: Driver :> es => DaemonPackage -> Eff es (Path Abs)
serviceFilePath p = do
  os <- drvOS
  case os of
    Linux _ -> do
      local <- drvLocal
      pure $ local </> "share" </> "systemd" </> "user" </> (daemonName p <> ".service")
    MacOS -> do
      home <- drvHome
      pure $ home </> "Library" </> "LaunchAgents" </> (daemonName p <> ".plist")

registerLinux :: Driver :> es => DaemonPackage -> Eff es ()
registerLinux p = do
  drvRun $ "systemctl" :| ["--user", "daemon-reload"]
  drvRun $ "systemctl" :| ["--user", "start", daemonName p]

registerMacOS :: Driver :> es => DaemonPackage -> Eff es ()
registerMacOS p = do
  path <- serviceFilePath p
  drvRun $ "launchctl" :| ["load", path.text]

-- | Install daemon service
daemonInstall :: App es => DaemonPackage -> Eff es ()
daemonInstall p = do
  path <- serviceFilePath p
  content <- serviceFileContent p
  drvWriteFile path content
  trkAdd p path

  os <- drvOS
  case os of
    Linux _ -> registerLinux p
    MacOS -> registerMacOS p

daemonLocalVersion :: Driver :> es => DaemonPackage -> Eff es (Maybe Text)
daemonLocalVersion p = do
  path <- serviceFilePath p
  exists <- drvIsFile path
  pure $ if exists then Just (Text.unwords $ toList p.daemon) else Nothing

daemonRemoteVersion :: Driver :> es => DaemonPackage -> Eff es Text
daemonRemoteVersion p = pure $ cmd p

instance Package DaemonPackage where
  localVersion = daemonLocalVersion
  remoteVersion = daemonRemoteVersion
  install = installWithPost daemonInstall
