import pytest

from mybox.package.installer.flatpak import Flatpak

from .base import PackageArgs, PackageTestBase


class TestFlatpak(PackageTestBase):
    affects_system = True

    prerequisites = [Flatpak.FLATPAK]

    async def constructor_args(self) -> PackageArgs:
        return {
            "flatpak": "org.videolan.VLC",
        }

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("This test is only applicable on Linux.")

    async def check_installed_command(self):
        return ["vlc", "--version"]

    check_installed_output = "NVIM"
