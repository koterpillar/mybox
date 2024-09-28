from pathlib import Path

import pytest

from mybox.package.shell import Shell
from mybox.state import DB

from .base import (
    CI,
    DOCKER,
    DummyTracker,
    PackageArgs,
    PackageTestBase,
    requires_driver,
)
from .driver import TestDriver


class ShellTestBase(PackageTestBase):
    shell: str = "/bin/sh"

    async def constructor_args(self) -> PackageArgs:
        return {"shell": self.shell, "root": self.root}

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
        if CI and not DOCKER:
            pytest.skip(
                "Cannot test normal user's shell without Docker on GitHub Actions."
            )
        return await super().test_installs(make_driver)


class TestRootShell(ShellTestBase):
    root = True


class TestCustomShell(TestShell):
    shell = "/bin/whoami"

    async def check_installed(self):
        # log in as the user to check that the right shell is called
        username = await self.check_driver.username()
        output = await self.driver.with_root(True).run_output("su", username)
        # whoami (as the shell) will output the username
        assert output == username


@pytest.mark.trio
async def test_errors_when_not_found(make_driver: TestDriver):
    package = Shell(
        shell=Path("/bin/xxxxxxxxxxxx"),
        root=False,
        driver=make_driver,
        db=DB.temporary(),
    )

    with pytest.raises(ValueError, match="does not exist"):
        await package.install(tracker=DummyTracker())


@pytest.mark.trio
async def test_errors_when_not_executable(make_driver: TestDriver):
    package = Shell(
        shell=Path("/etc/shells"),
        root=False,
        driver=make_driver,
        db=DB.temporary(),
    )

    with pytest.raises(ValueError, match="is not executable"):
        await package.install(tracker=DummyTracker())
