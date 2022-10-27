from abc import abstractmethod

from .base import PackageArgs, PackageTestBase


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


class TestPip(DjangoTestBase):
    @property
    def installation_method(self) -> str:
        return "pip"


class TestPipx(DjangoTestBase):
    @property
    def installation_method(self) -> str:
        return "pipx"

    prerequisites = PackageTestBase.PIPX
