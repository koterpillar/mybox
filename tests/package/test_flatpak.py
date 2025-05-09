import pytest

from ..base import DOCKER
from .base import PackageArgs, PackageTestBase


class TestFlatpak(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {
            "flatpak": "org.videolan.VLC",
        }

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("This test is only applicable on Linux.")

        if DOCKER:
            pytest.skip(
                "Flatpak tests require systemd, which is not available in Docker."
            )

    async def check_installed_command(self):
        return ["flatpak", "run", "org.videolan.VLC", "--version"]

    check_installed_output = "VLC version"
