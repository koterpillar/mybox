from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path

from ..state import INSTALLED_FILES, InstalledFile, Storage
from .base import Package


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


class Tracked(Package, ABC):
    @abstractmethod
    async def install_tracked(self, *, tracker: Tracker) -> None:
        pass

    async def install(self) -> None:
        installed_files = INSTALLED_FILES(self.db)

        current: set[InstalledFile] = set()
        previous: set[InstalledFile] = set(installed_files.find(package=self.name))

        tracker = Tracker(
            package=self.name,
            storage=installed_files,
            current=current,
            previous=previous,
        )

        await self.install_tracked(tracker=tracker)

        for removed in previous - current:
            await self.driver.rm(removed.path_)
            installed_files.delete(path=removed.path)

        await super().install()
