import pytest

from tests.package.driver import RootCheckDriver

from .base import CI, PackageArgs, RootPackageTestBase


class ShellTestBase(RootPackageTestBase):
    async def constructor_args(self, driver: RootCheckDriver) -> PackageArgs:
        return {"shell": "/bin/sh", "root": self.root}

    affects_system = True

    async def check_installed_command(self, driver: RootCheckDriver):
        return [
            "sh",
            "-c",
            f"cat /etc/passwd | grep {await self.check_driver(driver).username()}",
        ]

    check_installed_output = "/bin/sh"


class TestShell(ShellTestBase):
    @pytest.mark.trio
    async def test_installs(self, driver: RootCheckDriver):
        if (await self.os()).switch(linux=False, macos=CI):
            pytest.skip("Cannot test normal user's shell on macOS on GitHub Actions.")
        return await super().test_installs(driver)


class TestRootShell(ShellTestBase):
    root = True
