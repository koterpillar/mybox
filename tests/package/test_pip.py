from pathlib import Path
from typing import Optional

import pytest

from mybox.driver import LocalDriver
from mybox.package import PipxPackage
from mybox.state import DB

from .base import DOCKER, PackageArgs, PackageTestBase


class NoPypiPipxPackage(PipxPackage):
    async def _get_pypi_version(self) -> Optional[str]:
        return None


async def _test_pip_version(pip_class: type[PipxPackage]):
    package = pip_class(pipx="django", driver=LocalDriver(), db=DB(":memory:"))
    version = await package.get_remote_version()
    assert version >= "4.1.5"

    non_existent = pip_class(
        pipx="xxxxxxxxxxxx", driver=LocalDriver(), db=DB(":memory:")
    )

    with pytest.raises(Exception, match="Cannot find latest version"):
        await non_existent.get_remote_version()


@pytest.mark.trio
async def test_remote_version():
    await _test_pip_version(PipxPackage)


@pytest.mark.trio
async def test_remote_version_from_index():
    await _test_pip_version(NoPypiPipxPackage)


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
            await self.check_driver.home() / ".local" / "pipx",
        }
