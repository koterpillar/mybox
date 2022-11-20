import os
from pathlib import Path

import pytest

from mybox.installed_files import track_files
from mybox.package import Links

from .base import (
    DestinationPackageTestBase,
    PackageArgs,
    RootPackageTestBase,
    requires_driver,
)
from .driver import TestDriver


class LinksTestBase(DestinationPackageTestBase, RootPackageTestBase):
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


class TestLinks(LinksTestBase):
    @pytest.mark.trio
    @requires_driver
    async def test_links_removed(
        self, make_driver: TestDriver, tmp_path: Path  # pylint:disable=unused-argument
    ):
        await self.install_prerequisites()
        db = self.setup_db()

        source = tmp_path / "source"
        source.mkdir()

        async def make_package() -> Links:
            return Links(
                links=str(source),
                destination=await self.destination(),
                db=db,
                driver=self.driver,
            )

        async def list_files() -> list[str]:
            output = await self.check_driver.run_output("ls", await self.destination())
            return output.split()

        for name in ["one", "two"]:
            (source / name).touch()

        package = await make_package()
        async with track_files(
            db=db, driver=self.driver, package=package.name
        ) as tracker:
            await package.install(tracker=tracker)

        assert await list_files() == ["one", "two"]

        (source / "two").unlink()
        (source / "three").touch()

        package = await make_package()
        async with track_files(
            db=db, driver=self.driver, package=package.name
        ) as tracker:
            await package.install(tracker=tracker)

        assert await list_files() == ["one", "three"]


class TestShallowLinks(LinksTestBase):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {"shallow": True}


class TestDotLinks(LinksTestBase):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {"dot": True}

    destination_files = [".myfile", ".deep/space/nine/ncc-1701.txt"]


class TestRootLinks(LinksTestBase):
    root = True
