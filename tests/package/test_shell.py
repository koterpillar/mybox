import pytest

from .base import CI, PackageArgs, RootPackageTestBase, requires_driver
from .driver import TestDriver


class ShellTestBase(RootPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"shell": "/bin/sh", "root": self.root}

    affects_system = True

    async def check_installed_command(self):
        return [
            "sh",
            "-c",
            f"cat /etc/passwd | grep {await self.check_driver.username()}",
        ]

    check_installed_output = "/bin/sh"


class TestShell(ShellTestBase):
    @pytest.mark.trio
    @requires_driver
    async def test_installs(self, make_driver: TestDriver):
        if (await self.os()).switch(linux=False, macos=CI):
            pytest.skip("Cannot test normal user's shell on macOS on GitHub Actions.")
        return await super().test_installs(make_driver)


class TestRootShell(ShellTestBase):
    root = True
