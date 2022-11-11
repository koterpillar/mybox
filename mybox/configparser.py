from configparser import ConfigParser as BaseConfigParser
from io import StringIO


class ConfigParser(BaseConfigParser):
    def to_string(self) -> str:
        contents = StringIO()
        self.write(contents, space_around_delimiters=False)
        return contents.getvalue()

    @classmethod
    def from_string(cls, config: str) -> "ConfigParser":
        parser = cls()
        parser.read_string(config)
        return parser
