module Mybox.DesktopFile (
  DesktopFile (..),
  parseDesktopFile,
  renderDesktopFile,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Mybox.Prelude

-- | The fields of a desktop entry mybox uses.
data DesktopFile = DesktopFile
  { name :: Text
  -- ^ @Name@: the name shown by application launchers.
  , exec :: Text
  -- ^ @Exec@: the command to run.
  , icon :: Maybe Text
  -- ^ @Icon@: the name of an icon in the hicolor theme, if any.
  }
  deriving (Eq, Generic, Ord, Show)

-- | Parse a desktop entry file. Only the main @[Desktop Entry]@ group is
-- read: comments, @[Desktop Action]@ groups and other groups are ignored.
parseDesktopFile :: Text -> DesktopFile
parseDesktopFile contents =
  DesktopFile
    { name = Map.findWithDefault "" "Name" properties
    , exec = Map.findWithDefault "" "Exec" properties
    , icon = Map.lookup "Icon" properties
    }
 where
  properties :: Map Text Text
  properties = Map.fromList $ do
    -- Only the lines of the main group, wherever it appears in the file.
    line <-
      takeWhile (not . ("[" `Text.isPrefixOf`))
        . drop 1
        . dropWhile (/= "[Desktop Entry]")
        $ map Text.strip (Text.lines contents)
    -- Skip blank lines and comments.
    guard $ not (Text.null line)
    guard $ not ("#" `Text.isPrefixOf` line)
    let (key, rest) = Text.breakOn "=" line
    value <- maybe [] pure $ Text.stripPrefix "=" rest
    pure (key, value)

-- | Render a desktop entry file. Keys mybox doesn't model are given values
-- suitable for a minimal, valid entry.
renderDesktopFile :: DesktopFile -> Text
renderDesktopFile file =
  Text.unlines $
    [ "[Desktop Entry]"
    , "Type=Application"
    , "Name=" <> file.name
    , "Exec=" <> file.exec
    ]
      <> maybe [] (\icon -> ["Icon=" <> icon]) file.icon
      <> ["Terminal=false"]
