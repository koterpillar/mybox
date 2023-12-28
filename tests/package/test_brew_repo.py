import pytest

from .base import PackageArgs, PackageTestBase


class TestCaskVersions(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {
            "brew_tap": "homebrew/cask-versions",
        }

    async def check_installed_command(self):
        return ["brew", "info", "iterm2-beta"]

    check_installed_output = "iterm2.com"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=False, macos=True):
            pytest.skip("This test is only applicable on Fedora.")
