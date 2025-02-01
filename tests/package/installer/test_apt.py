import pytest

from mybox.package.installer.apt import Apt

from ...package.driver import TestDriver


class TestApt:
    @pytest.fixture
    async def apt(self, make_driver: TestDriver):
        (await make_driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution in {"debian", "ubuntu"}
                else pytest.skip("Apt is only available on Debian and Ubuntu")
            ),
            macos=lambda: pytest.skip("Apt is only available on Linux"),
        )
        return Apt(make_driver)

    @pytest.mark.trio
    async def test_installed_version(self, apt: Apt):
        version = await apt.installed_version("git")
        # Git should be installed as a prerequisite
        assert version is not None, "git is not installed"
        # Remove epoch
        if version.startswith("1:"):
            version = version[2:]
        assert "2.0" <= version <= "99"

    @pytest.mark.trio
    async def test_remote_version(self, apt: Apt):
        assert "8.10" <= await apt.latest_version("ghc") <= "99"

    @pytest.mark.trio
    async def test_non_existent_package(self, apt: Apt):
        with pytest.raises(ValueError, match="zzzzzzzz"):
            await apt.latest_version("zzzzzzzz")
