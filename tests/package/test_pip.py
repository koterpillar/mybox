from pathlib import Path

import pytest

from mybox.driver import LocalDriver
from mybox.package import PipxPackage
from mybox.state import DB

from .base import DOCKER, PackageArgs, PackageTestBase


@pytest.mark.trio
async def test_remote_version():
    package = PipxPackage(pipx="django", driver=LocalDriver(), db=DB.temporary())
    version = await package.get_remote_version()
    assert version >= "4.1.5"

    non_existent = PipxPackage(
        pipx="xxxxxxxxxxxx", driver=LocalDriver(), db=DB.temporary()
    )

    with pytest.raises(Exception, match="Cannot find latest version"):
        await non_existent.get_remote_version()


class TestPipx(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"pipx": "django"}

    async def check_installed_command(self):
        return ["django-admin", "help"]

    check_installed_output = (
        "Type 'django-admin help <subcommand>' for help on a specific subcommand."
    )

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not DOCKER:
            pytest.skip("Cannot test pip packages without Docker.")

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.home() / ".pipx",
            await self.check_driver.home() / ".local" / "pipx" / ".cache",
            await self.check_driver.home() / ".local" / "pipx" / "shared",
            await self.check_driver.home() / ".local" / "pipx" / "logs",
            await self.check_driver.home() / ".shiv",
        }
