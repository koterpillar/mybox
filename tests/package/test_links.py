import os

from tests.package.driver import RootCheckDriver

from .base import DestinationPackageTestBase, PackageArgs, RootPackageTestBase


class TestLinks(DestinationPackageTestBase, RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {
            "links": f"{os.path.dirname(__file__)}/test_links_content",
            "destination": self.destination,
            "root": self.root,
        }

    async def check_installed_command(self, driver: RootCheckDriver):
        return ["cat", await self.destination(driver) / self.destination_file]

    check_installed_output = "Linked file"

    destination_file = "myfile"


class TestDotLinks(TestLinks):
    @property
    def constructor_args(self) -> PackageArgs:
        return super().constructor_args | {"dot": True}

    destination_file = ".myfile"


class TestRootLinks(TestLinks):
    root = True
