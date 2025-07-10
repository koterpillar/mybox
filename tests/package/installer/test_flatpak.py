import pytest

from mybox.manager import Manager
from mybox.package import parse_package
from mybox.package.installer.flatpak import Flatpak
from mybox.state import DB

from ...base import DOCKER
from ...package.driver import TestDriver


class TestFlatpak:
    @pytest.fixture
    async def flatpak(self, make_driver: TestDriver, tmp_path):
        (await make_driver.os()).switch_(
            linux=lambda os: None,
            macos=lambda: pytest.skip("Flatpak is only available on Linux"),
        )

        if DOCKER:
            pytest.skip(
                "Flatpak tests require systemd, which is not available in Docker."
            )

        result = Flatpak(make_driver)

        # Install Flatpak as a prerequisite
        db = DB.temporary()
        manager = Manager(driver=make_driver, db=db, data_path=tmp_path)
        pkg = parse_package(Flatpak.FLATPAK, db=db, driver=make_driver)
        install_result = await manager.install_packages([pkg])
        install_result.raise_for_failures()

        return result

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
