from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator

from .driver import Driver
from .state import DB, Storage, storage


@dataclass
class InstalledFile:
    path: str

    @property
    def path_(self) -> Path:
        return Path(self.path)

    @path_.setter
    def path_(self, value: Path) -> None:
        self.path = str(value)


INSTALLED_FILES = storage("installed_file", InstalledFile)


@dataclass
class Tracker:
    storage: Storage[InstalledFile]
    current: set[Path]
    previous: set[Path]

    def track(self, target: Path) -> None:
        self.current.add(target)
        if target not in self.previous:
            self.storage.append(InstalledFile(path=str(target)))


@asynccontextmanager
async def track_files(db: DB, driver: Driver) -> AsyncIterator[Tracker]:
    installed_files = INSTALLED_FILES(db)

    current: set[Path] = set()
    previous: set[Path] = {f.path_ for f in installed_files.find()}

    yield Tracker(
        storage=installed_files,
        current=current,
        previous=previous,
    )

    for removed in previous - current:
        await driver.rm(removed)
        installed_files.delete(path=str(removed))
