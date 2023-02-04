from abc import ABCMeta
from functools import cached_property
from pathlib import Path

from ..driver import Driver
from ..utils import async_cached
from .base import Package


class Root(Package, metaclass=ABCMeta):
    root: bool

    def __init__(self, root: bool = False, **kwargs) -> None:
        super().__init__(**kwargs)
        self.root = root

    @cached_property
    def driver(self) -> Driver:
        return super().driver.with_root(self.root)

    @async_cached
    async def local(self) -> Path:
        if self.root:
            return Path("/usr/local")
        else:
            return await self.driver.local()
