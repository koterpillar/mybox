module Mybox.Config.CommandLine where

import Options.Applicative

import Mybox.Prelude

data CommandLine = CommandLine | CLVersion | CLPackageHelp
  deriving (Eq, Show)

optionsParser :: Parser CommandLine
optionsParser =
  flag' CLVersion (long "version" <> help "Show version")
    <|> flag' CLPackageHelp (long "package-help" <> help "Describe the package definition format")
    <|> pure CommandLine

optionsInfo :: ParserInfo CommandLine
optionsInfo = info (optionsParser <**> helper) fullDesc

parseArgs :: IOE :> es => Eff es CommandLine
parseArgs = liftIO $ execParser optionsInfo
