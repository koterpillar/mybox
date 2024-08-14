from abc import ABC, abstractmethod
from functools import cached_property
from typing import AsyncIterable, Optional

from pydantic import BaseModel, ConfigDict, Field

from ..driver import Driver
from ..parallel import gather_
from ..state import DB
from ..tracker import Tracker
from ..utils import allow_singular, async_cached, matches_if_specified


class Package(BaseModel, ABC):
    model_config = ConfigDict(
        frozen=True, arbitrary_types_allowed=True, ignored_types=(cached_property,)
    )

    def __hash__(self) -> int:
        """Allow hashing to pass when gathering async results."""
        return hash(id(self))

    name_: Optional[str] = Field(default=None, alias="name")

    os: Optional[list[str]] = None
    os_val = allow_singular("os")

    distribution: Optional[list[str]] = None
    distribution_val = allow_singular("distribution")

    db: DB
    driver_: Driver = Field(..., alias="driver")

    root: bool = False

    @property
    def driver(self) -> Driver:
        return self.driver_.with_root(self.root)

    @property
    def name(self) -> str:
        return self.name_ or self.derive_name()

    def derive_name(self) -> str:
        raise ValueError("Package name not set.")

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
        remote, local = await gather_(self.remote_version, self.local_version)
        return remote == local

    @abstractmethod
    async def install(self, *, tracker: Tracker) -> None:
        pass

    async def applicable(self) -> bool:
        os = await self.driver.os()
        return os.switch_(
            linux=lambda linux: matches_if_specified(self.os, "linux")
            and matches_if_specified(self.distribution, linux.distribution),
            macos=lambda: matches_if_specified(self.os, "darwin"),
        )

    async def prerequisites(self) -> AsyncIterable["Package"]:
        return
        yield
