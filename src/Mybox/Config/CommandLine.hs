module Mybox.Config.CommandLine where

import Data.Text qualified as Text
import Options.Applicative

import Mybox.Aeson hiding (Parser)
import Mybox.Package.Some
import Mybox.Prelude

data CommandLine
  = CmdInline
      { inline :: SomePackage
      }
  | CmdDirectory
      { directory :: Maybe (Path AnyAnchor)
      }
  deriving (Show)

readJSON :: FromJSON a => ReadM a
readJSON = eitherReader $ eitherDecodeStrictText . Text.pack

readPath :: Anchor a => ReadM (Path a)
readPath = eitherReader $ mkPath_ . Text.pack

parseInline :: Parser SomePackage
parseInline =
  option readJSON $
    short 'i'
      <> long "inline"
      <> metavar "TEXT"
      <> help "Inline parameter with text value"

parseRegular :: Parser (Maybe (Path AnyAnchor))
parseRegular =
  optional $
    option readPath $
      short 'd' <> long "directory" <> metavar "DIR" <> help "Configuration directory"

optionsParser :: Parser CommandLine
optionsParser = CmdInline <$> parseInline <|> CmdDirectory <$> parseRegular

optionsInfo :: ParserInfo CommandLine
optionsInfo = info (optionsParser <**> helper) fullDesc

parseArgs :: IOE :> es => Eff es CommandLine
parseArgs = liftIO $ execParser optionsInfo
