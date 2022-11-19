import hashlib
from dataclasses import dataclass
from functools import cached_property
from pathlib import Path
from typing import Iterator

from ..state import Storage, storage
from ..utils import Some, unsome_
from .destination import Destination
from .manual_version import ManualVersion


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


class Links(ManualVersion, Destination):
    def __init__(
        self,
        links: str,
        dot: bool = False,
        shallow: bool = False,
        only: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.source = Path(links).absolute()
        self.dot = dot
        self.shallow = shallow
        self.only = unsome_(only)

    @cached_property
    def installed_files(self) -> Storage[InstalledFile]:
        return INSTALLED_FILES(self.db)

    @property
    def name(self) -> str:
        return f"links-{self.source}-{self.destination_}-{self.dot}-{self.root}"

    def all_paths(self) -> Iterator[Path]:
        if not self.source.is_dir():
            raise ValueError(f"Source is not a directory: {self.source}")

        if self.shallow:
            return (path for path in self.source.glob("*"))
        else:
            return (path for path in self.source.rglob("*") if path.is_file())

    def paths(self) -> Iterator[Path]:
        for path in self.all_paths():
            if self.only and path.name not in self.only:
                continue
            yield path

    async def get_remote_version(self) -> str:
        m = hashlib.sha256()
        for path in self.paths():
            m.update(str(path).encode())
        return m.hexdigest()

    async def install(self) -> None:
        destination = await self.destination()

        currently_installed: set[Path] = set()
        previously_installed: set[Path] = {
            f.path_ for f in self.installed_files.find(package=self.name)
        }

        for path in self.paths():
            target = path.relative_to(self.source)
            if self.dot:
                first_part, *parts = target.parts
                target = Path("." + first_part, *parts)
            target = destination.joinpath(target)

            await self.driver.link(path, target)
            currently_installed.add(target)
            if target not in previously_installed:
                self.installed_files.append(
                    InstalledFile(package=self.name, path=str(target))
                )

        for removed in previously_installed - currently_installed:
            await self.driver.rm(removed)
            self.installed_files.delete(package=self.name, path=str(removed))

        await super().install()
