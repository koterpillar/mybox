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
        version = await flatpak.latest_version("org.gnome.Shotwell")
        # version is remote:hash
        parts = version.split(":")
        assert len(parts) == 2
        assert parts[0] in {"fedora", "flathub"}
        assert len(parts[1]) == 12

    @pytest.mark.trio
    async def test_non_existent_package(self, flatpak: Flatpak):
        with pytest.raises(ValueError, match="zzzzzzzz"):
            await flatpak.latest_version("zzzzzzzz")
