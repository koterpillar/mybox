from pathlib import Path

from mybox.driver import LocalDriver
from mybox.package import Daemon, parse_packages
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


async def test_parse_packages():
    driver = LocalDriver()
    db = DB.temporary()

    args = {
        "name": "Test",
        "daemon": ["echo", "hello"],
    }

    # Package with no implementation field are kept
    normal_packages = parse_packages([args], db=db, driver=driver)
    assert len(normal_packages) == 1
    assert isinstance(normal_packages[0], Daemon)

    # Package with implementation=python are kept
    explicit_implementation = parse_packages(
        [{**args, "$implementation": "python"}], db=db, driver=driver
    )
    assert len(explicit_implementation) == 1
    assert isinstance(explicit_implementation[0], Daemon)

    # Package with implementation=haskell is filtered out
    package_haskell = parse_packages(
        [{**args, "$implementation": "haskell"}], db=db, driver=driver
    )
    assert package_haskell == []

    # Packages moved to Haskell implementation are filtered out
    special_packages = parse_packages(
        [{"repo": "test/test"}, {"pipx": "test"}, {"url": "http://example.com"}, args],
        db=db,
        driver=driver,
    )
    assert len(special_packages) == 1
    assert isinstance(special_packages[0], Daemon)
