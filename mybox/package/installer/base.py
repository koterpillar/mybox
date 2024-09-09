from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Generic, Optional, Protocol, TypeVar

import trio

from ...driver import Driver


class Installer(ABC):
    def __init__(self, driver: Driver) -> None:
        self.driver = driver
        super().__init__()

    @abstractmethod
    async def install(self, package: str) -> None:
        pass

    @abstractmethod
    async def upgrade(self, package: str) -> None:
        pass

    @abstractmethod
    async def installed_version(self, package: str) -> Optional[str]:
        pass

    @abstractmethod
    async def latest_version(self, package: str) -> str:
        pass


class PVP(Protocol):
    @property
    def installed(self) -> Optional[str]: ...
    @property
    def latest(self) -> str: ...


PV = TypeVar("PV", bound=PVP)


@dataclass
class PackageVersionInfo:
    installed: Optional[str]
    latest: str


class PackageCacheInstaller(Generic[PV], Installer, ABC):
    cache: Optional[dict[str, PV]]

    def __init__(self, driver: Driver) -> None:
        self.cache = None
        self.lock = trio.Lock()
        super().__init__(driver)

    @abstractmethod
    async def get_package_info(self, package: Optional[str]) -> dict[str, PV]:
        pass

    async def package_info(self, package: str) -> PV:
        async with self.lock:
            if self.cache is None:
                self.cache = await self.get_package_info(None)

        async with self.lock:
            if package not in self.cache:
                self.cache.update(await self.get_package_info(package))

        try:
            return self.cache[package]
        except KeyError:
            raise ValueError(f"Unknown package: {package}") from None

    async def installed_version(self, package: str) -> Optional[str]:
        info = await self.package_info(package)
        return info.installed

    async def latest_version(self, package: str) -> str:
        info = await self.package_info(package)
        return info.latest

    async def invalidate_cache(self, package: str) -> None:
        async with self.lock:
            if self.cache:
                self.cache.pop(package, None)

    async def install(self, package: str) -> None:
        await super().install(package)
        await self.invalidate_cache(package)

    async def upgrade(self, package: str) -> None:  # pragma: no cover
        # Can't install an old version of a package in tests
        await super().upgrade(package)
        await self.invalidate_cache(package)
