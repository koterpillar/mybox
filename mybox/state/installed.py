from dataclasses import dataclass
from pathlib import Path

from .base import storage


@dataclass(frozen=True)
class InstalledFile:
    path: str
    package: str
    root: bool

    @property
    def path_(self) -> Path:
        return Path(self.path)


INSTALLED_FILES = storage("installed_file", InstalledFile)
