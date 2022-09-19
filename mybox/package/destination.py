from abc import ABCMeta
from pathlib import Path

from ..fs import home
from .base import Package


class Destination(Package, metaclass=ABCMeta):
    destination: Path

    def __init__(self, destination: str, root: bool = False, **kwargs) -> None:
        self.root = root
        if destination.startswith("/"):
            self.destination = Path(destination)
        else:
            if self.root:
                self.destination = Path("~root").expanduser() / destination
            else:
                self.destination = home() / destination
        super().__init__(**kwargs)
