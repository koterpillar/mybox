import pytest

from mybox.driver import LocalDriver
from mybox.package.installer import Brew
from mybox.utils import run


class TestBrew:
    @pytest.fixture
    async def brew(self):
        driver = LocalDriver()
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
