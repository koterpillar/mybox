from abc import ABC
from pathlib import Path

from pydantic import Field

from ..utils import async_cached
from .base import Package


class Destination(Package, ABC):
    destination_: Path = Field(..., alias="destination")

    @async_cached
    async def destination(self) -> Path:
        return await self.driver.home() / self.destination_
