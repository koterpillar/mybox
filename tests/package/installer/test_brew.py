import pytest

from mybox.package.installer.brew import Brew

from ...package.driver import TestDriver


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
        assert "formula_9." <= await brew.latest_version("ghc") <= "formula_99"

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
