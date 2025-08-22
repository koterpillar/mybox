module Mybox.Config.CommandLine (
  CommandLine (..),
  optionsInfo,
  parseArgs,
) where

import Data.Text qualified as Text
import Options.Applicative

import Mybox.Aeson hiding (Parser)
import Mybox.Package.Some
import Mybox.Prelude

data CommandLine
  = CmdInline
      { inline :: [SomePackage]
      , installSet :: Maybe Text
      }
  | CmdDirectory
      { directory :: Maybe (Path AnyAnchor)
      , installSet :: Maybe Text
      }
  deriving (Eq, Show)

readYAML :: FromJSON a => ReadM a
readYAML = eitherReader $ yamlDecodeEither . Text.pack

readPath :: Anchor a => ReadM (Path a)
readPath = eitherReader $ mkPath_ . Text.pack

inlineP :: Parser [SomePackage]
inlineP =
  many $
    option readYAML $
      short 'p'
        <> long "package"
        <> metavar "YAML"
        <> help "Package definition"

installSetP :: Parser (Maybe Text)
installSetP =
  optional $
    strOption $
      long "install-set"
        <> metavar "SET"
        <> help "Install set to use for tracking packages"

directoryP :: Parser (Maybe (Path AnyAnchor))
directoryP =
  optional $
    option readPath $
      short 'd' <> long "directory" <> help "Configuration directory"

optionsParser :: Parser CommandLine
optionsParser = inlineOP <|> directoryOP
 where
  inlineOP = CmdInline <$> inlineP <*> installSetP
  directoryOP = CmdDirectory <$> directoryP <*> installSetP

optionsInfo :: ParserInfo CommandLine
optionsInfo = info (optionsParser <**> helper) fullDesc

parseArgs :: IOE :> es => Eff es CommandLine
parseArgs = liftIO $ execParser optionsInfo
