from dataclasses import dataclass
from pathlib import Path

from .base import Storage, storage


@dataclass(frozen=True)
class InstalledFile:
    package: str
    path: str
    root: bool

    @property
    def path_(self) -> Path:
        return Path(self.path)


INSTALLED_FILES = storage("installed_file", InstalledFile)


@dataclass
class Tracker:
    package: str
    storage: Storage[InstalledFile]
    current: set[InstalledFile]
    previous: set[InstalledFile]

    def track(self, target: Path, *, root: bool = False) -> None:
        installed = InstalledFile(package=self.package, path=str(target), root=root)
        if installed not in self.current:
            self.current.add(installed)
            if installed not in self.previous:
                self.storage.append(installed)
