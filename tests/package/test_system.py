import pytest

from ..base import CI, DOCKER
from .base import PackageArgs, PackageTestBase


class TestRipGrep(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "ripgrep"}

    async def check_installed_command(self):
        return ["rg", "--help"]

    check_installed_output = "ripgrep"


class TestCask(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "alacritty"}

    async def check_installed_command(self):
        return ["alacritty", "--version"]

    check_installed_output = "alacritty 0"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda _: pytest.skip("Cask test is only applicable on macOS"),
            macos=lambda: None,
        )


class TestInteractiveDeb(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "tzdata"}

    async def check_installed_command(self):
        return ["cat", "/usr/share/doc/tzdata/copyright"]

    check_installed_output = "Internet Assigned Numbers Authority"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution in {"debian", "ubuntu"}
                else pytest.skip("This test is only applicable on Debian and Ubuntu")
            ),
            macos=lambda: pytest.skip("This test is only applicable on Linux"),
        )


class TestVirtualPackage(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "g++"}

    async def check_installed_command(self):
        return ["g++", "--version"]

    check_installed_output = "Free Software Foundation, Inc."

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("This test is only applicable on Linux.")


class TestService(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"system": "apache2", "service": "apache2"}

    async def check_installed_command(self):
        return ["ps", "aux"]

    check_installed_output = "apache2"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not CI:
            pytest.skip("Test affects local system.")
        (await self.driver.os()).switch_(
            linux=lambda _: None,
            macos=lambda: pytest.skip("Service test is only applicable on Linux"),
        )
        if DOCKER:
            pytest.skip("Daemon relies on systemd, which is not available in Docker.")
