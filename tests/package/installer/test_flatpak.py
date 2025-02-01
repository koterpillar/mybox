import pytest

from mybox.package.installer.flatpak import Flatpak

from ...package.driver import TestDriver


class TestFlatpak:
    @pytest.fixture
    async def flatpak(self, make_driver: TestDriver):
        (await make_driver.os()).switch_(
            linux=lambda os: None,
            macos=lambda: pytest.skip("Flatpak is only available on Linux"),
        )
        return Flatpak(make_driver)

    @pytest.mark.trio
    async def test_remote_version(self, flatpak: Flatpak):
        assert "0.32" <= await flatpak.latest_version("org.gnome.Shotwell") <= "999"

    @pytest.mark.trio
    async def test_non_existent_package(self, flatpak: Flatpak):
        with pytest.raises(ValueError, match="zzzzzzzz"):
            await flatpak.latest_version("zzzzzzzz")
