from abc import ABCMeta
from dataclasses import dataclass
from functools import cached_property
from typing import Optional

from ..state import Storage, storage
from .base import Package


@dataclass
class Version:
    version: str


VERSIONS = storage("version", Version)


class ManualVersion(Package, metaclass=ABCMeta):
    @cached_property
    def versions(self) -> Storage[Version]:
        return VERSIONS(self.db)

    @property
    def cached_version(self) -> Optional[str]:
        try:
            return self.versions[self.name].version
        except KeyError:
            return None

    async def local_version(self) -> Optional[str]:
        return self.cached_version

    async def cache_version(self) -> None:
        self.versions[self.name] = Version(version=await self.get_remote_version())

    async def install(self) -> None:
        await super().install()
        await self.cache_version()
