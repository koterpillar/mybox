import os
from abc import ABCMeta, abstractmethod
from typing import Any

from .base import PackageTestBase


class LinksTestBase(PackageTestBase, metaclass=ABCMeta):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return {
            "links": f"{os.path.dirname(__file__)}/test_links_content",
            "destination": "config",
        }

    @property
    def check_installed_command(self) -> list[str]:
        return ["cat", f"{self.driver.home()}/{self.destination_file}"]

    check_installed_output = "Linked file"

    @property
    @abstractmethod
    def destination_file(self) -> str:
        pass


class TestLinks(LinksTestBase):
    destination_file = "config/myfile"


class TestDotLinks(LinksTestBase):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return super().constructor_args | {"dot": True}

    destination_file = "config/.myfile"
