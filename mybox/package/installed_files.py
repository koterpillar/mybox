from contextlib import asynccontextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterator, Callable

from ..driver import Driver
from ..state import DB, storage


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


@asynccontextmanager
async def track_files(
    db: DB, driver: Driver, package: str
) -> AsyncIterator[Callable[[Path], None]]:
    installed_files = INSTALLED_FILES(db)

    currently_installed: set[Path] = set()
    previously_installed: set[Path] = {
        f.path_ for f in installed_files.find(package=package)
    }

    def track_file(path: Path) -> None:
        currently_installed.add(path)
        if path not in previously_installed:
            installed_files.append(InstalledFile(package=package, path=str(path)))

    yield track_file

    for removed in previously_installed - currently_installed:
        await driver.rm(removed)
        installed_files.delete(package=package, path=str(removed))
