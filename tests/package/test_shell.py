from typing import Iterable

import pytest

from mybox.driver import LocalDriver

from .base import CI, PackageArgs, RootPackageTestBase


class ShellTestBase(RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {"shell": "/bin/sh", "root": self.root}

    affects_system = True

    @property
    def check_installed_command(self) -> Iterable[str]:
        return ["sh", "-c", f"cat /etc/passwd | grep {self.test_driver.username}"]

    check_installed_output = "/bin/sh"


class TestShell(ShellTestBase):
    def test_installs(self):
        if LocalDriver().os.switch(linux=False, macos=CI):
            pytest.skip("Cannot test normal user's shell on macOS on GitHub Actions.")
        return super().test_installs()


class TestRootShell(ShellTestBase):
    root = True
