module Mybox.Driver.Platform (module Mybox.Platform, drvArchitecture, drvOS) where

import Data.Text qualified as Text

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.Platform
import Mybox.Prelude

drvArchitecture :: Driver :> es => Eff es Architecture
drvArchitecture =
  drvRunOutput ("uname" :| ["-m"]) >>= \case
    "x86_64" -> pure X86_64
    "aarch64" -> pure Aarch64
    "arm64" -> pure Aarch64
    arch -> terror $ "Unsupported architecture: " <> arch

drvOS :: Driver :> es => Eff es OS
drvOS = do
  osStr <- drvRunOutput ("uname" :| [])
  case osStr of
    "Linux" -> do
      distributionStr <- parseOsRelease <$> drvReadFile "/etc/os-release"
      distribution <- case distributionStr of
        "debian" -> pure $ Debian "debian"
        "ubuntu" -> pure $ Debian "ubuntu"
        "fedora" -> pure Fedora
        _ -> terror $ "Unsupported Linux distribution: " <> distributionStr
      pure $ Linux distribution
     where
      parseOsRelease :: Text -> Text
      parseOsRelease contents = fromMaybe (terror "Failed to parse /etc/os-release") $ listToMaybe $ do
        line <- Text.lines contents
        [k, v] <- pure $ Text.splitOn "=" line
        guard (k == "ID")
        if Text.isPrefixOf "\"" v && Text.isSuffixOf "\"" v
          then pure $ Text.drop 1 $ Text.dropEnd 1 v
          else pure v
    "Darwin" -> pure MacOS
    _ -> terror $ "Unsupported OS: " <> osStr
