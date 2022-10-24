from typing import Iterable

import pytest

from .base import CI, PackageArgs, RootPackageTestBase


class ShellTestBase(RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {"shell": "/bin/sh", "root": self.root}

    affects_system = True

    async def check_installed_command(self) -> Iterable[str]:
        return [
            "sh",
            "-c",
            f"cat /etc/passwd | grep {await self.test_driver.username()}",
        ]

    check_installed_output = "/bin/sh"


class TestShell(ShellTestBase):
    async def test_installs(self):
        if (await self.os()).switch(linux=False, macos=CI):
            pytest.skip("Cannot test normal user's shell on macOS on GitHub Actions.")
        return await super().test_installs()


class TestRootShell(ShellTestBase):
    root = True
