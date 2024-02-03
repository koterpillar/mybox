from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator

from .driver import Driver
from .state import DB, INSTALLED_FILES, InstalledFile, Storage


class Tracker(ABC):
    @abstractmethod
    def track(self, target: Path, *, root: bool = False) -> None:
        pass


@dataclass
class DBTracker(Tracker):
    storage: Storage[InstalledFile]
    stale: set[InstalledFile]
    current: set[InstalledFile]

    def track(self, target: Path, *, root: bool = False) -> None:
        installed = InstalledFile(path=str(target), root=root)
        if installed not in self.current:
            self.current.add(installed)
            if installed in self.stale:
                self.stale.remove(installed)
            else:
                self.storage.append(installed)

    @classmethod
    @asynccontextmanager
    async def tracking(cls, *, driver: Driver, db: DB) -> AsyncIterator[Tracker]:
        installed_files = INSTALLED_FILES(db)

        stale = set(installed_files.find())
        current: set[InstalledFile] = set()

        tracker = cls(
            storage=installed_files,
            stale=stale,
            current=current,
        )

        try:
            yield tracker
        finally:
            for installed_file in stale:
                await driver.with_root(installed_file.root).rm(installed_file.path_)
                installed_files.delete(path=installed_file.path)
