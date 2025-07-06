module Mybox.Driver.Platform where

import Data.Text qualified as Text

import Mybox.Driver.Class
import Mybox.Driver.Ops
import Mybox.Prelude

data Architecture = X86_64 | Aarch64 deriving (Eq, Ord, Show)

drvArchitecture :: Driver :> es => Eff es Architecture
drvArchitecture =
  drvRunOutput ("uname" :| ["-m"]) >>= \case
    "x86_64" -> pure X86_64
    "aarch64" -> pure Aarch64
    "arm64" -> pure Aarch64
    arch -> terror $ "Unsupported architecture: " <> arch

data OS = Linux {distribution :: Text} | MacOS deriving (Eq, Ord, Show)

drvOS :: Driver :> es => Eff es OS
drvOS = do
  osStr <- drvRunOutput ("uname" :| [])
  case osStr of
    "Linux" -> do
      distribution <- parseOsRelease <$> drvReadFile "/etc/os-release"
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
