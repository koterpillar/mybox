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
            "url": "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-40.noarch.rpm",
        }

    async def check_installed_command(self):
        return ["cat", "/etc/yum.repos.d/rpmfusion-free.repo"]

    check_installed_output = "RPM Fusion for Fedora"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution == "fedora"
                else pytest.skip("YUM repo test is only applicable  on Fedora")
            ),
            macos=lambda: pytest.skip("YUM repo test is only applicable on Linux"),
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
