import pytest

from .base import PackageArgs, PackageTestBase


class TestBrewRepo(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        # Chosen from https://docs.brew.sh/Interesting-Taps-and-Forks
        return {
            "brew_tap": "denji/nginx",
        }

    async def check_installed_command(self):
        return ["brew", "info", "nginx-full"]

    check_installed_output = "HTTP(S) server"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=False, macos=True):
            pytest.skip("This test is only applicable on macOS.")
