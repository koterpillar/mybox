from abc import ABCMeta
from pathlib import Path

from ..utils import async_cached
from .root import Root


class Destination(Root, metaclass=ABCMeta):
    def __init__(self, destination: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.destination_ = destination

    @async_cached
    async def destination(self) -> Path:
        return await self.driver.home() / self.destination_
