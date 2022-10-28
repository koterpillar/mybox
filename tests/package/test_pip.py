from abc import abstractmethod

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


class TestPipx(DjangoTestBase):
    @property
    def installation_method(self) -> str:
        return "pipx"

    prerequisites = PackageTestBase.PIPX
