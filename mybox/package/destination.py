from abc import ABCMeta
from pathlib import Path

from .root import Root


class Destination(Root, metaclass=ABCMeta):
    destination: Path

    def __init__(self, destination: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.destination = self.driver.home() / destination
