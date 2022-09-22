from abc import ABCMeta
from functools import cached_property

from ..fs import FS
from .base import Package


class Root(Package, metaclass=ABCMeta):
    root: bool

    def __init__(self, root: bool = False, **kwargs) -> None:
        super().__init__(**kwargs)
        self.root = root

    @cached_property
    def fs(self) -> FS:
        return super().fs.with_root(self.root)
