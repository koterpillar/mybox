import os

from mybox.driver import Driver
from tests.package.driver import RootCheckDriver

from .base import DestinationPackageTestBase, PackageArgs, RootPackageTestBase


class TestLinks(DestinationPackageTestBase, RootPackageTestBase):
    async def constructor_args(self, driver: RootCheckDriver) -> PackageArgs:
        return {
            "links": f"{os.path.dirname(__file__)}/test_links_content",
            "destination": await self.destination(driver),
            "root": self.root,
        }

    async def check_installed_command(self, driver: RootCheckDriver):
        return ["cat", await self.destination(driver) / self.destination_file]

    check_installed_output = "Linked file"

    destination_file = "myfile"


class TestDotLinks(TestLinks):
    async def constructor_args(self, driver: RootCheckDriver) -> PackageArgs:
        return await super().constructor_args(driver) | {"dot": True}

    destination_file = ".myfile"


class TestRootLinks(TestLinks):
    root = True
