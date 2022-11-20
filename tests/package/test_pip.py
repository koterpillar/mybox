from abc import abstractmethod
from pathlib import Path

import pytest

from .base import DOCKER, PackageArgs, PackageTestBase


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


class TestPip(DjangoTestBase):
    @property
    def installation_method(self) -> str:
        return "pip"

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.home() / ".local" / "bin",
            await self.check_driver.home() / ".local" / "lib",
        }


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
