import pytest

from mybox.fs import LocalFS
from mybox.package.system import Brew
from mybox.utils import run


class TestBrew:
    @pytest.fixture
    def brew(self):
        fs = LocalFS()
        fs.os.switch(
            linux=lambda: pytest.skip("brew is only available on macOS"),
            macos=lambda: None,
        )()
        return Brew(fs)

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
