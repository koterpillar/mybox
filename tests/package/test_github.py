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
        if (await self.driver.os()).switch(linux=True, macos=False):
            local = await self.local()

            # Check desktop file
            desktop_file = await self.check_driver.run_output(
                "cat", f"{local}/share/applications/nvim.desktop"
            )
            assert "Name=Neovim" in desktop_file

            # Check icon
            assert await self.check_driver.is_file(
                f"{local}/share/icons/hicolor/128x128/apps/nvim.png"
            )


class TestRootNeovim(TestNeovim):
    root = True
    affects_system = True


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
