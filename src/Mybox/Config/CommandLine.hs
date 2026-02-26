module Mybox.Config.CommandLine where

import Options.Applicative

import Mybox.Prelude

data CommandLine = CommandLine | CLVersion
  deriving (Eq, Show)

optionsParser :: Parser CommandLine
optionsParser =
  flag' CLVersion (long "version")
    <|> pure CommandLine

optionsInfo :: ParserInfo CommandLine
optionsInfo = info (optionsParser <**> helper) fullDesc

parseArgs :: IOE :> es => Eff es CommandLine
parseArgs = liftIO $ execParser optionsInfo
