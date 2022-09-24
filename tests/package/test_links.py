import os
from abc import ABCMeta, abstractmethod
from typing import Any

from .base import DestinationPackageTestBase, RootPackageTestBase


class LinksTestBase(DestinationPackageTestBase, RootPackageTestBase, metaclass=ABCMeta):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return {
            "links": f"{os.path.dirname(__file__)}/test_links_content",
            "destination": self.destination,
            "root": self.root,
        }

    @property
    def check_installed_command(self) -> list[str]:
        return ["cat", str(self.destination / self.destination_file)]

    check_installed_output = "Linked file"

    @property
    @abstractmethod
    def destination_file(self) -> str:
        pass


class TestLinks(LinksTestBase):
    destination_file = "myfile"


class TestDotLinks(LinksTestBase):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return super().constructor_args | {"dot": True}

    destination_file = ".myfile"
