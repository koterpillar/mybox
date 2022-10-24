from .base import PackageTestBase


class TestYarn(PackageTestBase):
    constructor_args = {
        "url": "https://yarnpkg.com/latest.tar.gz",
        "binary": "yarn",
        "binary_wrapper": True,
        "strip": 1,
    }

    prerequisites = PackageTestBase.NODE

    async def check_installed_command(self):
        return ["yarn", "--help"]

    check_installed_output = "Usage: yarn"
