from mybox.driver import Driver

from .base import PackageArgs, PackageTestBase, RootPackageTestBase


class TestNeovim(RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {
            "repo": "neovim/neovim",
            "strip": 1,
            "binary": "nvim",
            "app": "nvim",
            "root": self.root,
        }

    @property
    def test_driver(self) -> Driver:
        # Ordinary users should be able to use the package even if installed as root
        return super().test_driver.with_root(False)

    check_installed_command = ["nvim", "--version"]

    check_installed_output = "NVIM"


class TestRootNeovim(TestNeovim):
    root = True
    affects_system = True


class TestExa(PackageTestBase):
    constructor_args = {
        "repo": "ogham/exa",
        "binary": "exa",
    }

    check_installed_command = ["exa", "--version"]

    check_installed_output = "exa - list files"


class TestAmmonite(PackageTestBase):
    constructor_args = {
        "repo": "com-lihaoyi/Ammonite",
        "regex": r"^2\.13-[0-9]+\.[0-9]+\.[0-9]+-bootstrap$",
        "raw": "amm",
        "raw_executable": True,
        "binary": "amm",
    }

    prerequisites = PackageTestBase.JAVA

    check_installed_command = ["amm", "--version"]

    check_installed_output = "Ammonite REPL"
