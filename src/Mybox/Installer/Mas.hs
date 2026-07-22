{-# LANGUAGE AllowAmbiguousTypes #-}

module Mybox.Installer.Mas where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Driver
import Mybox.Effects
import Mybox.Installer.Class
import Mybox.Package.Queue
import Mybox.Prelude

runMas :: forall s es r. (App es, IsSystemPackage s) => (Args -> Eff es r) -> [Text] -> Eff es r
runMas act args = do
  drvOS >>= \case
    MacOS -> pure ()
    _ -> error "mas installer is only available on macOS"
  queueInstall $ mkSystemPackage_ @s "mas" []
  act $ "mas" :| args

masInstall :: forall s es. (App es, IsSystemPackage s) => Text -> Eff es ()
masInstall package = runMas @s drvRun ["purchase", package]

masUpgrade :: forall s es. (App es, IsSystemPackage s) => Text -> Eff es ()
masUpgrade package = runMas @s drvRun ["upgrade", package]

{-
\$ mas list
682658836   GarageBand   (10.4.12)
408981434   iMovie       (10.4.3)
409183694   Keynote      (14.4)
409203825   Numbers      (14.4)
490179405   Okta Verify  (9.52.0)
409201541   Pages        (14.4)

\$ mas outdated
497799835 Xcode (15.4 -> 16.0)
640199958 Developer (10.6.5 -> 10.6.6)
-}
parseList :: Text -> Map Text PackageVersion
parseList = Map.fromList . map parseLine . Text.lines
 where
  parseLine ln = fromMaybe (error $ "Invalid mas list line format: " <> show ln) $ do
    pkg <- listToMaybe (Text.words ln)
    let version = Text.dropEnd 1 $ Text.dropWhile (/= '(') ln
    case Text.words version of
      [v] -> Just (pkg, PackageVersion{installed = Just v, latest = v})
      [vInstalled, "->", vLatest] ->
        Just
          (pkg, PackageVersion{installed = Just vInstalled, latest = vLatest})
      _ -> Nothing

masList :: forall s es. (App es, IsSystemPackage s) => Maybe Text -> Eff es (Map Text PackageVersion)
masList pkg = fmap parseList $ runMas @s drvRunOutput $ "list" : toList pkg

masOutdated :: forall s es. (App es, IsSystemPackage s) => Maybe Text -> Eff es (Map Text PackageVersion)
masOutdated pkg = fmap parseList $ runMas @s drvRunOutput $ "outdated" : toList pkg

{-
\$ mas info 899247664
TestFlight 4.0.1 [Free]
By: Apple Pty Limited
Released: 2025-10-08
Minimum OS: 13.0
Size: 6.7 MB
From: https://apps.apple.com/au/app/testflight/id899247664?uo=4
-}
parseInfo :: Text -> Text -> Map Text PackageVersion
parseInfo pkg info = Map.singleton pkg $ PackageVersion{..}
 where
  installed = Nothing
  latest = case Text.words info of
    (_name : version : _) -> version
    _ -> error $ "Invalid mas info format: " <> show info

masPackageInfo :: forall s es. (App es, IsSystemPackage s) => Maybe Text -> Eff es (Map Text PackageVersion)
masPackageInfo pkg = do
  local <- Map.union <$> masOutdated @s pkg <*> masList @s pkg
  available <- case pkg of
    Nothing -> pure mempty
    Just pkg' -> parseInfo pkg' <$> runMas @s drvRunOutput ["info", pkg']
  pure $ Map.union local available

mas :: forall s. IsSystemPackage s => Installer
mas =
  Installer
    { storeKey = "mas"
    , install_ = masInstall @s
    , installURL = iURLNotImplemented
    , upgrade_ = masUpgrade @s
    , getPackageInfo = masPackageInfo @s
    }
