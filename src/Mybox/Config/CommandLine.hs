module Mybox.Config.CommandLine where

import Options.Applicative

import Mybox.Prelude

data CommandLine = CommandLine {selfUpdate :: Bool} | CLVersion
  deriving (Eq, Generic, Show)

withoutSelfUpdate :: CommandLine -> CommandLine
withoutSelfUpdate commandLine@CommandLine{} = commandLine{selfUpdate = False}
withoutSelfUpdate CLVersion = CLVersion

optionsParser :: Parser CommandLine
optionsParser =
  flag' CLVersion (long "version")
    <|> (CommandLine <$> flag True False (long "no-self-update" <> help "Do not update Mybox before installing packages"))

commandLineArgs :: CommandLine -> [Text]
commandLineArgs CLVersion = ["--version"]
commandLineArgs commandLine = ["--no-self-update" | not commandLine.selfUpdate]

optionsInfo :: ParserInfo CommandLine
optionsInfo = info (optionsParser <**> helper) fullDesc

parseArgs :: IOE :> es => Eff es CommandLine
parseArgs = liftIO $ execParser optionsInfo
