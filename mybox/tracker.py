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

    @staticmethod
    @asynccontextmanager
    async def tracking(*, driver: Driver, db: DB) -> AsyncIterator["ManagerTracker"]:
        installed_files = INSTALLED_FILES(db)

        stale = set(installed_files.find())
        current: set[InstalledFile] = set()

        tracker = ManagerTracker(
            storage=installed_files,
            stale=stale,
            current=current,
        )

        try:
            yield tracker
        finally:
            current_paths = set(current_file.path for current_file in current)

            for stale_file in stale:
                if stale_file.path not in current_paths:
                    await driver.with_root(stale_file.root).rm(stale_file.path_)
                installed_files.delete(package=stale_file.package, path=stale_file.path)


@dataclass
class ManagerTracker:
    storage: Storage[InstalledFile]
    stale: set[InstalledFile]
    current: set[InstalledFile]

    def skip(self, package: str) -> None:
        intact = {
            installed_file
            for installed_file in self.stale
            if installed_file.package == package
        }
        self.stale -= intact
        self.current |= intact

    def track(self, package: str) -> Tracker:
        return PackageTracker(parent=self, name=package)


@dataclass
class PackageTracker(Tracker):
    parent: ManagerTracker
    name: str

    def track(self, target: Path, *, root: bool = False) -> None:
        installed = InstalledFile(path=str(target), package=self.name, root=root)
        if installed not in self.parent.current:
            self.parent.current.add(installed)
            if installed in self.parent.stale:
                self.parent.stale.remove(installed)
            else:
                self.parent.storage.append(installed)
