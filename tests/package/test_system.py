import pytest

from .base import PackageArgs, PackageTestBase


class TestRipGrep(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "ripgrep"}

    async def check_installed_command(self):
        return ["rg", "--help"]

    check_installed_output = "ripgrep"


class TestRPMFusion(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {
            "system": "rpmfusion-free-release",
            "url": "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-38.noarch.rpm",
        }

    async def check_installed_command(self):
        return ["cat", "/etc/yum.repos.d/rpmfusion-free.repo"]

    check_installed_output = "RPM Fusion for Fedora"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch_(
            linux=lambda os: os.distribution == "fedora", macos=False
        ):
            pytest.skip("This test is only applicable on Fedora.")


class TestInteractiveDeb(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "tzdata"}

    async def check_installed_command(self):
        return ["cat", "/usr/share/doc/tzdata/copyright"]

    check_installed_output = "Internet Assigned Numbers Authority"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch_(
            linux=lambda os: os.distribution in ("debian", "ubuntu"), macos=False
        ):
            pytest.skip("This test is only applicable on Debian-based systems.")


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
