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
        return [
            "cat",
            *[
                await self.destination() / file_name
                for file_name in self.destination_files
            ],
        ]

    check_installed_output = "Linked file\nEnterprise"

    destination_files: list[str] = ["myfile", "deep/space/nine/ncc-1701.txt"]


class TestShallowLinks(TestLinks):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {"shallow": True}


class TestDotLinks(TestLinks):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {"dot": True}

    destination_files = [".myfile", ".deep/space/nine/ncc-1701.txt"]


class TestRootLinks(TestLinks):
    root = True
