import pytest
import pytest_asyncio

from mybox.driver import LocalDriver
from mybox.package.system import Brew
from mybox.utils import run

from .base import PackageTestBase


class TestBrew:
    @pytest_asyncio.fixture
    async def brew(self):
        driver = LocalDriver()
        (await driver.os()).switch(
            linux=lambda: pytest.skip("brew is only available on macOS"),
            macos=lambda: None,
        )()
        return Brew(driver)

    @pytest.fixture(autouse=True)
    def brew_tap_fonts(self, brew):  # pylint:disable=unused-argument
        run("brew", "tap", "homebrew/cask-fonts")

    def test_formula_version(self, brew):
        assert "8.10" <= brew.latest_version("ghc") <= "99"

    def test_cask_version(self, brew):
        assert "4.12" <= brew.latest_version("homebrew/cask/docker") <= "99"

    def test_font_version(self, brew):
        assert "6.2" <= brew.latest_version("homebrew/cask-fonts/font-fira-code")

    def test_not_existing_formula(self, brew):
        with pytest.raises(Exception, match="zzzzzzzz"):
            brew.latest_version("zzzzzzzz")

    def test_non_tapped_cask(self, brew):
        with pytest.raises(Exception, match="homebrew/cask-zzzzzzz/yyyyyyy"):
            brew.latest_version("homebrew/cask-zzzzzzz/yyyyyyy")


class TestRipGrep(PackageTestBase):
    affects_system = True

    constructor_args = {
        "name": "ripgrep",
    }

    async def check_installed_command(self):
        return ["rg", "--help"]

    check_installed_output = "ripgrep"


class TestRPMFusion(PackageTestBase):
    affects_system = True

    constructor_args = {
        "name": "rpmfusion-free-release",
        "url": "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-36.noarch.rpm",
    }

    async def check_installed_command(self):
        return ["cat", "/etc/yum.repos.d/rpmfusion-free.repo"]

    check_installed_output = "RPM Fusion for Fedora"

    async def check_applicable(self) -> None:
        if not (await self.driver.os()).switch_(
            linux=lambda os: os.distribution == "fedora", macos=False
        ):
            pytest.skip("This test is only applicable on Fedora.")
