from abc import ABC, abstractmethod
from functools import cached_property
from pathlib import Path
from typing import Optional

from pydantic import AliasChoices, AliasPath, BaseModel, ConfigDict, Field

from ..driver import Driver
from ..state import DB
from ..utils import (
    allow_singular,
    async_cached,
    matches_if_specified,
)

PackageArgs = dict[
    str, str | bool | int | Path | list[str] | dict[str, str | list[str]]
]


class Package(BaseModel, ABC):
    model_config = ConfigDict(
        frozen=True,
        arbitrary_types_allowed=True,
        ignored_types=(cached_property,),
        extra="forbid",
    )

    def __hash__(self) -> int:
        """Allow hashing to pass when gathering async results."""
        return hash(id(self))

    name_: Optional[str] = Field(default=None, alias="name")

    os: Optional[list[str]] = Field(
        default=None, validation_alias=AliasChoices(AliasPath("$if", "os"), "os")
    )
    os_val = allow_singular("os")

    distribution: Optional[list[str]] = None
    distribution_val = allow_singular("distribution")

    db: DB
    driver: Driver

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
        remote = await self.remote_version()
        local = await self.local_version()
        return remote == local

    @abstractmethod
    async def install(self) -> None:
        pass

    async def applicable(self) -> bool:
        os = await self.driver.os()
        return os.switch_(
            linux=lambda linux: (
                matches_if_specified(self.os, "linux")
                or matches_if_specified(self.os, linux.distribution)
            )
            and matches_if_specified(self.distribution, linux.distribution),
            macos=lambda: matches_if_specified(self.os, "darwin"),
        )
