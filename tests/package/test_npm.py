from pathlib import Path

import pytest

from mybox.driver import LocalDriver
from mybox.package import NpmPackage
from mybox.state import DB

from .base import PackageArgs, PackageTestBase


@pytest.mark.trio
async def test_remote_version():
    package = NpmPackage(npm="express", driver=LocalDriver(), db=DB.temporary())
    version = await package.get_remote_version()
    assert version >= "4.18.2"

    non_existent = NpmPackage(
        npm="xxxxxxxxxxxx", driver=LocalDriver(), db=DB.temporary()
    )

    with pytest.raises(Exception, match="Cannot find latest version"):
        await non_existent.get_remote_version()


class TestExpress(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"npm": "express-generator", "binary": "express"}

    async def check_installed_command(self):
        return ["express", "--help"]

    check_installed_output = "engine support"

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {await self.check_driver.home() / ".npm"}
