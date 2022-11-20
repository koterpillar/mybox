from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator

from .driver import Driver
from .state import DB, Storage, storage


@dataclass(frozen=True)
class InstalledFile:
    path: str
    root: bool

    @property
    def path_(self) -> Path:
        return Path(self.path)


INSTALLED_FILES = storage("installed_file", InstalledFile)


@dataclass
class Tracker:
    storage: Storage[InstalledFile]
    current: set[InstalledFile]
    previous: set[InstalledFile]

    def track(self, target: Path, *, root: bool = False) -> None:
        installed = InstalledFile(path=str(target), root=root)
        if installed not in self.current:
            self.current.add(installed)
            if installed not in self.previous:
                self.storage.append(installed)


@asynccontextmanager
async def track_files(db: DB, driver: Driver) -> AsyncIterator[Tracker]:
    installed_files = INSTALLED_FILES(db)

    current: set[InstalledFile] = set()
    previous: set[InstalledFile] = set(installed_files.find())

    yield Tracker(
        storage=installed_files,
        current=current,
        previous=previous,
    )

    for removed in previous - current:
        await driver.with_root(removed.root).rm(removed.path_)
        installed_files.delete(path=removed.path)
