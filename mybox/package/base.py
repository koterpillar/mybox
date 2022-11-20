from abc import ABCMeta, abstractmethod
from typing import Optional

from ..driver import Driver
from ..state import DB
from ..utils import Some, async_cached, gather, unsome_


class Package(metaclass=ABCMeta):
    os: Optional[list[str]]
    distribution: Optional[list[str]]

    def __init__(
        self,
        *,
        db: DB,
        driver: Driver,
        os: Some[str] = None,
        distribution: Some[str] = None,
    ) -> None:
        self.db = db
        self.driver_ = driver
        self.os = unsome_(os)
        self.distribution = unsome_(distribution)

    @property
    def driver(self) -> Driver:
        return self.driver_

    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @abstractmethod
    async def get_remote_version(self) -> str:
        pass

    @async_cached
    async def remote_version(self) -> str:
        return await self.get_remote_version()

    @abstractmethod
    async def local_version(self) -> Optional[str]:
        pass

    async def is_installed(self) -> bool:
        remote, local = await gather(self.remote_version, self.local_version)
        return remote == local

    @abstractmethod
    async def install(self) -> None:
        pass

    async def applicable(self) -> bool:
        def check_os(name: str) -> bool:
            return self.os is None or name in self.os

        os = await self.driver.os()
        return os.switch_(
            linux=lambda linux: check_os("linux")
            and (self.distribution is None or linux.distribution in self.distribution),
            macos=check_os("darwin"),
        )

    async def ensure(self) -> bool:
        if not await self.applicable():
            return False
        if await self.is_installed():
            return False
        await self.install()
        return True
