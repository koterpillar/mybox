from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator

from ..driver import Driver
from ..state import DB, Storage, storage


@dataclass
class InstalledFile:
    package: str
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
    package: str
    driver: Driver
    storage: Storage[InstalledFile]
    current: set[Path]
    previous: set[Path]

    def track(self, target: Path) -> None:
        self.current.add(target)
        if target not in self.previous:
            self.storage.append(InstalledFile(package=self.package, path=str(target)))

    async def link(self, source: Path, target: Path) -> None:
        await self.driver.link(source, target)
        self.track(target)


@asynccontextmanager
async def track_files(db: DB, driver: Driver, package: str) -> AsyncIterator[Tracker]:
    installed_files = INSTALLED_FILES(db)

    current: set[Path] = set()
    previous: set[Path] = {f.path_ for f in installed_files.find(package=package)}

    yield Tracker(
        package=package,
        driver=driver,
        storage=installed_files,
        current=current,
        previous=previous,
    )

    for removed in previous - current:
        await driver.rm(removed)
        installed_files.delete(package=package, path=str(removed))
