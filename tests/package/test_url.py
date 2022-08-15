from .base import PackageTestBase


class TestYarn(PackageTestBase):
    constructor_args = {
        "url": "https://yarnpkg.com/latest.tar.gz",
        "binary": "yarn",
        "binary_wrapper": True,
        "strip": 1,
    }

    check_installed_command = ["yarn", "--help"]

    check_installed_output = "Usage: yarn"
