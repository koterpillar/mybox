import pytest

from mybox.package.system import Brew
from mybox.utils import CURRENT_OS


class TestBrew:
    @pytest.fixture(autouse=True)
    def brew(self):
        if CURRENT_OS != "darwin":
            pytest.skip("brew is only available on macOS")
        return Brew()

    def test_formula_version(self, brew):
        assert "8.10" <= brew.latest_version("ghc") <= "99"

    def test_cask_version(self, brew):
        assert "4.12" <= brew.latest_version("homebrew/cask/docker") <= "99"
