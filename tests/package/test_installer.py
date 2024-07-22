import pytest

from mybox.package.installer import DNF, Apt, Brew

from ..package.driver import TestDriver


class TestBrew:
    @pytest.fixture
    async def brew(self, make_driver: TestDriver):
        (await make_driver.os()).switch_(
            linux=lambda _: pytest.skip("brew is only available on macOS"),
            macos=lambda: None,
        )
        return Brew(make_driver)

    @pytest.mark.trio
    async def test_formula_version(self, brew: Brew):
        assert "8.10" <= await brew.latest_version("ghc") <= "99"

    @pytest.mark.trio
    async def test_cask_version(self, brew: Brew):
        assert "0.13.2" <= await brew.latest_version("alacritty") <= "99"

    @pytest.mark.trio
    async def test_cask_version_preferred_over_formula(self, brew: Brew):
        # Formula version is 27.x, cask follows Docker Desktop version
        assert "4." <= await brew.latest_version("docker") <= "9."

    @pytest.mark.trio
    async def test_not_existing_formula(self, brew: Brew):
        with pytest.raises(Exception, match="zzzzzzzz"):
            await brew.latest_version("zzzzzzzz")

    @pytest.mark.trio
    async def test_non_tapped_cask(self, brew: Brew):
        with pytest.raises(Exception, match="homebrew/cask-zzzzzzz/yyyyyyy"):
            await brew.latest_version("homebrew/cask-zzzzzzz/yyyyyyy")


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
    async def test_non_existent_package(self, apt: Apt):
        with pytest.raises(ValueError, match="zzzzzzzz"):
            await apt.latest_version("zzzzzzzz")
