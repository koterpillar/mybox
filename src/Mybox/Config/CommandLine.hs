module Mybox.Config.CommandLine where

import Options.Applicative

import Mybox.Prelude

data CommandLine = CommandLine {selfUpdate :: Bool} | CLVersion | CLPackageHelp
  deriving (Eq, Generic, Show)

withoutSelfUpdate :: CommandLine -> CommandLine
withoutSelfUpdate commandLine@CommandLine{} = commandLine{selfUpdate = False}
withoutSelfUpdate commandLine = commandLine

optionsParser :: Parser CommandLine
optionsParser =
  flag' CLVersion (long "version" <> help "Show version")
    <|> flag' CLPackageHelp (long "package-help" <> help "Describe the package definition format")
    <|> (CommandLine <$> flag True False (long "no-self-update" <> help "Do not update Mybox before installing packages"))

commandLineArgs :: CommandLine -> [Text]
commandLineArgs CLVersion = ["--version"]
commandLineArgs CLPackageHelp = ["--package-help"]
commandLineArgs CommandLine{selfUpdate} = ["--no-self-update" | not selfUpdate]

optionsInfo :: ParserInfo CommandLine
optionsInfo = info (optionsParser <**> helper) fullDesc

parseArgs :: IOE :> es => Eff es CommandLine
parseArgs = liftIO $ execParser optionsInfo
