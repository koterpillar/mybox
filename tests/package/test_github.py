from mybox.driver import Driver
from tests.package.driver import RootCheckDriver

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

    def check_driver(self, driver: RootCheckDriver) -> Driver:
        # Ordinary users should be able to use the package even if installed as root
        return super().check_driver(driver).with_root(False)

    async def check_installed_command(self, driver: Driver):
        return ["nvim", "--version"]

    check_installed_output = "NVIM"


class TestRootNeovim(TestNeovim):
    root = True
    affects_system = True


class TestExa(PackageTestBase):
    constructor_args = {
        "repo": "ogham/exa",
        "binary": "exa",
    }

    async def check_installed_command(self, driver: Driver):
        return ["exa", "--version"]

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

    async def check_installed_command(self, driver: Driver):
        return ["amm", "--version"]

    check_installed_output = "Ammonite REPL"
