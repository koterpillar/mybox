from configparser import ConfigParser as BaseConfigParser
from io import StringIO


class ConfigParser(BaseConfigParser):
    def optionxform(self, optionstr: str) -> str:
        return optionstr

    def to_string(self) -> str:
        contents = StringIO()
        self.write(contents, space_around_delimiters=False)
        return contents.getvalue()
