import os
from typing import Any

from .base import DestinationPackageTestBase, RootPackageTestBase


class TestLinks(DestinationPackageTestBase, RootPackageTestBase):
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

    destination_file = "myfile"


class TestDotLinks(TestLinks):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return super().constructor_args | {"dot": True}

    destination_file = ".myfile"


class TestRootLinks(TestLinks):
    root = True
