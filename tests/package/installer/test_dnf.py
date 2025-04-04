import pytest

from mybox.package.installer import DNF

from ...package.driver import TestDriver


class TestDNF:
    @pytest.fixture
    async def dnf(self, make_driver: TestDriver):
        (await make_driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution == "fedora"
                else pytest.skip("DNF is only available on Fedora")
            ),
            macos=lambda: pytest.skip("DNF is only available on Linux"),
        )
        return DNF(make_driver)

    @pytest.mark.trio
    async def test_installed_version(self, dnf: DNF):
        version = await dnf.installed_version("git")
        # Git should be installed as a prerequisite
        assert version is not None, "git is not installed"
        assert "2.0" <= version <= "99"

    @pytest.mark.trio
    async def test_remote_version(self, dnf: DNF):
        assert "8.10" <= await dnf.latest_version("ghc") <= "99"

    @pytest.mark.trio
    async def test_not_existing_package(self, dnf: DNF):
        with pytest.raises(ValueError, match="zzzzzzzz"):
            await dnf.latest_version("zzzzzzzz")
