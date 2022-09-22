from abc import ABCMeta
from pathlib import Path

from .base import Package


class Destination(Package, metaclass=ABCMeta):
    destination: Path

    def __init__(self, destination: str, root: bool = False, **kwargs) -> None:
        self.root = root
        self.destination = self.fs.home() / destination
        super().__init__(**kwargs)
