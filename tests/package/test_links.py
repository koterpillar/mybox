import os

from .base import DestinationPackageTestBase, PackageArgs, RootPackageTestBase


class TestLinks(DestinationPackageTestBase, RootPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "links": f"{os.path.dirname(__file__)}/test_links_content",
            "destination": await self.destination(),
            "root": self.root,
        }

    async def check_installed_command(self):
        return ["cat", await self.destination() / self.destination_file]

    check_installed_output = "Linked file"

    destination_file = "myfile"


class TestDotLinks(TestLinks):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {"dot": True}

    destination_file = ".myfile"


class TestRootLinks(TestLinks):
    root = True
