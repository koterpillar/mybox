import pytest

from mybox.driver import Driver

from .base import PackageArgs, PackageTestBase, RootPackageTestBase


class TestNeovim(RootPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "neovim/neovim",
            "strip": 1,
            "binary": "nvim",
            "app": "nvim",
            "root": self.root,
        }

    @property
    def check_driver(self) -> Driver:
        # Ordinary users should be able to use the package even if installed as root
        return super().check_driver.with_root(False)

    async def check_installed_command(self):
        return ["nvim", "--version"]

    check_installed_output = "NVIM"

    async def check_installed(self):
        await super().check_installed()
        await super().assert_desktop_file_exists("nvim", "Neovim")


class TestRootNeovim(TestNeovim):
    root = True
    affects_system = True


class TestKitty(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "kovidgoyal/kitty",
            "binary": "kitty",
            "app": "kitty",
        }

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("This test is only applicable on Linux.")

    async def check_installed_command(self):
        return ["kitty", "--version"]

    check_installed_output = "kitty"

    async def check_installed(self):
        await super().check_installed()
        await super().assert_desktop_file_exists("kitty", "kitty")


class TestExa(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "ogham/exa",
            "binary": "exa",
        }

    async def check_installed_command(self):
        return ["exa", "--version"]

    check_installed_output = "exa - list files"


class TestAmmonite(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "com-lihaoyi/Ammonite",
            "regex": r"^2\.13-[0-9]+\.[0-9]+\.[0-9]+-bootstrap$",
            "raw": "amm",
            "raw_executable": True,
            "binary": "amm",
        }

    prerequisites = PackageTestBase.JAVA

    async def check_installed_command(self):
        return ["amm", "--version"]

    check_installed_output = "Ammonite REPL"


class TestCura(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "Ultimaker/Cura",
            "include": "modern",
            "binary": "Ultimaker-Cura",
            "app": "cura",
        }

    async def check_installed_command(self):
        return ["Ultimaker-Cura", "--debug", "--version"]

    check_installed_output = "cura version"

    async def check_installed(self):
        await super().check_installed()
        await super().assert_desktop_file_exists("cura", "Cura")
