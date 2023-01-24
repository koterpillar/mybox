from abc import abstractmethod
from pathlib import Path
from typing import Optional

import pytest

from mybox.driver import LocalDriver
from mybox.package import PipPackage

from .base import DOCKER, PackageArgs, PackageTestBase


class NoPypiPipPackage(PipPackage):
    async def _get_pypi_version(self) -> Optional[str]:
        return None


async def _test_django_version(pip_class: type[PipPackage]):
    package = pip_class(pip="django", driver=LocalDriver(), db=None)
    version = await package.get_remote_version()
    assert version >= "4.1.5"


@pytest.mark.trio
async def test_remote_version():
    await _test_django_version(PipPackage)


@pytest.mark.trio
async def test_remote_version_from_index():
    await _test_django_version(NoPypiPipPackage)


class DjangoTestBase(PackageTestBase):
    @property
    @abstractmethod
    def installation_method(self) -> str:
        pass

    async def constructor_args(self) -> PackageArgs:
        return {self.installation_method: "django"}

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
            await self.check_driver.home() / ".local" / "bin",
            await self.check_driver.home() / ".local" / "lib",
        }


class TestPip(DjangoTestBase):
    @property
    def installation_method(self) -> str:
        return "pip"


class TestPipx(DjangoTestBase):
    @property
    def installation_method(self) -> str:
        return "pipx"

    prerequisites = PackageTestBase.PIPX

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.home() / ".pipx",
            await self.check_driver.home() / ".local" / "pipx",
        }
