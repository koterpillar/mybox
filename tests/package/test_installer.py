import pytest

from mybox.driver import Driver, LocalDriver
from mybox.package.installer import DNF, Brew
from mybox.utils import run

from ..base import DOCKER_IMAGE
from .driver import DockerDriver


async def make_driver() -> Driver:
    if DOCKER_IMAGE:
        return await DockerDriver.create(image=DOCKER_IMAGE)
    else:
        return LocalDriver()


class TestBrew:
    @pytest.fixture
    async def brew(self):
        driver = await make_driver()
        (await driver.os()).switch(
            linux=lambda: pytest.skip("brew is only available on macOS"),
            macos=lambda: None,
        )()
        return Brew(driver)

    @pytest.fixture
    async def brew_tap_fonts(self, brew):  # pylint:disable=unused-argument
        await run("brew", "tap", "homebrew/cask-fonts")

    @pytest.mark.trio
    async def test_formula_version(self, brew: Brew):
        assert "8.10" <= await brew.latest_version("ghc") <= "99"

    @pytest.mark.trio
    async def test_cask_version(self, brew: Brew):
        assert "4.12" <= await brew.latest_version("homebrew/cask/docker") <= "99"

    @pytest.mark.trio
    async def test_font_version(
        self, brew: Brew, brew_tap_fonts: None  # pylint:disable=unused-argument
    ):
        assert "6.2" <= await brew.latest_version("homebrew/cask-fonts/font-fira-code")

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
    async def dnf(self):
        driver = await make_driver()
        skip_reason = (await driver.os()).switch_(
            linux=lambda os: None
            if os.distribution == "fedora"
            else "DNF is only available on Fedora",
            macos="dnf is only available on Linux",
        )
        if skip_reason:
            pytest.skip(skip_reason)
        return DNF(driver)

    @pytest.mark.trio
    async def test_installed_version(self, dnf: DNF):
        version = await dnf.installed_version("git")
        # Git should be installed as a prerequisite
        assert version is not None, "git is not installed"
        assert "2.0" <= version <= "99"

    @pytest.mark.trio
    async def test_remote_version(self, dnf: DNF):
        assert "8.10" <= await dnf.latest_version("ghc") <= "99"
