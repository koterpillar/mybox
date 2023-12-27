import pytest

from mybox.driver import LocalDriver
from mybox.package.url import URLPackage
from mybox.state import DB

from .base import PackageArgs, PackageTestBase


class TestYarn(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "url": "https://yarnpkg.com/latest.tar.gz",
            "binary": "yarn",
            "binary_wrapper": True,
        }

    prerequisites = PackageTestBase.NODE

    async def check_installed_command(self):
        return ["yarn", "--help"]

    check_installed_output = "Usage: yarn"


def test_name():
    package = URLPackage(
        url="https://example.com/somewhere/thing.tar.gz",
        driver=LocalDriver(),
        db=DB.temporary(),
    )
    assert package.name == "example.com/thing"


def test_no_name():
    package = URLPackage(
        url={"format": "ignore", "base": "ignore"},
        driver=LocalDriver(),
        db=DB.temporary(),
    )

    with pytest.raises(ValueError):
        len(package.name)
