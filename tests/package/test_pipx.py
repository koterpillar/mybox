from pathlib import Path

import pytest

from mybox.driver import LocalDriver
from mybox.package import PipxPackage
from mybox.state import DB

from .base import PackageArgs, PackageTestBase


@pytest.mark.trio
async def test_remote_version():
    package = PipxPackage(pipx="django", driver=LocalDriver(), db=DB.temporary())
    version = await package.get_remote_version()
    assert version >= "4.1.5"


@pytest.mark.trio
async def test_remote_version_non_existent():
    non_existent = PipxPackage(
        pipx="xxxxxxxxxxxx", driver=LocalDriver(), db=DB.temporary()
    )

    with pytest.raises(Exception, match="Cannot find latest version"):
        await non_existent.get_remote_version()


@pytest.mark.trio
async def test_remote_version_git():
    package = PipxPackage(
        pipx="git+https://github.com/django/django.git",
        driver=LocalDriver(),
        db=DB.temporary(),
    )
    version = await package.get_remote_version()
    # Version will be a Git commit hash, which are 40 characters long.
    assert len(version) == 40


class TestPipx(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"pipx": "tqdm"}

    async def check_installed_command(self):
        return ["tqdm", "--help"]

    check_installed_output = "Usage:\n  tqdm"

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.home() / ".shiv",
            await self.check_driver.local() / "bin" / "pipx",
            await self.check_driver.local() / "share" / "pipx",
            await self.check_driver.local() / "state" / "pipx" / "log",
        }


class TestPipxGit(TestPipx):
    async def constructor_args(self) -> PackageArgs:
        return {"pipx": "git+https://github.com/tqdm/tqdm.git"}
