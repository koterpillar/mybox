from pathlib import Path

import pytest

from mybox.driver import LocalDriver
from mybox.package import Daemon
from mybox.state import DB

from ..base import PACKAGE_ROOT
from .base import DestinationPackageTestBase, PackageArgs


class PostTestBase(DestinationPackageTestBase):
    async def post_file(self) -> Path:
        return await self.destination() / "post"

    async def constructor_args(self) -> PackageArgs:
        return {
            "links": f"{PACKAGE_ROOT}/test/fixtures/links",
            "destination": await self.destination(),
            "root": self.root,
            "post": f"echo postinstall > {await self.post_file()}",
        }

    async def check_installed_command(self):
        return ["cat", await self.post_file()]

    check_installed_output = "postinstall"

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {await self.post_file()}


class TestPost(PostTestBase):
    pass


class TestRootPost(PostTestBase):
    root = True


@pytest.mark.trio
async def test_applicable_implementation():
    args = {
        "name": "Test",
        "daemon": ["echo", "hello"],
        "driver": LocalDriver(),
        "db": DB.temporary(),
    }

    # Package with no implementation field should be applicable
    package_no_impl = Daemon(**args)
    assert await package_no_impl.applicable()

    # Package with implementation=python should be applicable
    package_python = Daemon(**{**args, "$implementation": "python"})
    assert await package_python.applicable()

    # Package with implementation=haskell should NOT be applicable
    package_haskell = Daemon(**{**args, "$implementation": "haskell"})
    assert not await package_haskell.applicable()
