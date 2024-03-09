from configparser import ConfigParser as BaseConfigParser
from io import StringIO
from typing import Optional, TypeVar

CP = TypeVar("CP", bound="ConfigParser")


class ConfigParser(BaseConfigParser):
    def optionxform(self, optionstr: str) -> str:
        return optionstr

    def to_string(self) -> str:
        contents = StringIO()
        self.write(contents, space_around_delimiters=False)
        return contents.getvalue()

    @classmethod
    def from_string(cls: type[CP], config: str) -> CP:
        parser = cls(interpolation=None)
        parser.read_string(config)
        return parser


class DesktopEntry(ConfigParser):
    @property
    def icon(self) -> Optional[str]:
        return self["Desktop Entry"].get("Icon")
