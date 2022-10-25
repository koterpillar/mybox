from mybox.driver import Driver
from tests.package.driver import RootCheckDriver

from .base import PackageArgs, PackageTestBase


class TestYarn(PackageTestBase):
    async def constructor_args(self, driver: RootCheckDriver) -> PackageArgs:
        return {
            "url": "https://yarnpkg.com/latest.tar.gz",
            "binary": "yarn",
            "binary_wrapper": True,
            "strip": 1,
        }

    prerequisites = PackageTestBase.NODE

    async def check_installed_command(self, driver: Driver):
        return ["yarn", "--help"]

    check_installed_output = "Usage: yarn"
