from pathlib import Path

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
