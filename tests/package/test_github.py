from pathlib import Path

import pytest

from mybox.driver import Driver, LocalDriver
from mybox.package.github import GitHubPackage
from mybox.state import DB

from .base import PackageArgs, PackageTestBase, RootPackageTestBase


class TestNeovim(RootPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "neovim/neovim",
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
        await super().assert_desktop_file_exists(
            "nvim", name="Neovim", executable="nvim"
        )

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.local() / "state" / "nvim"
        }


class TestRootNeovim(TestNeovim):
    root = True
    affects_system = True


class TestKitty(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "kovidgoyal/kitty",
            "binary": "kitty",
            "app": "kitty",
            "exclude": "kitten",
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
        await super().assert_desktop_file_exists(
            "kitty", name="kitty", executable="kitty"
        )


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

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.home() / ".ammonite"
        }


class TestCura(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"repo": "Ultimaker/Cura", "exclude": "modern"}

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("This test is only applicable on Linux.")

    async def check_installed_command(self):
        return ["xvfb-run", "Ultimaker--Cura", "--debug", "--version"]

    check_installed_output = "cura version"

    async def check_installed(self):
        await super().check_installed()
        await super().assert_desktop_file_exists(
            "com.ultimaker.cura", name="UltiMaker Cura", executable="Ultimaker--Cura"
        )

    prerequisites = [
        {
            "system": "libgl1-mesa-glx",
            "distribution": ["debian", "ubuntu"],
        },
        {
            "system": "mesa-libGL",
            "distribution": "fedora",
        },
        {
            "system": "libegl1",
            "distribution": ["debian", "ubuntu"],
        },
        {
            "system": "mesa-libEGL",
            "distribution": "fedora",
        },
        {
            "system": "xvfb",
            "distribution": ["debian", "ubuntu"],
        },
        {
            "system": "xorg-x11-server-Xvfb",
            "distribution": "fedora",
        },
    ]


class TestGitHubCLI(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"repo": "cli/cli", "binary": "gh", "strip": 1}

    async def check_installed_command(self):
        return ["gh", "--version"]

    check_installed_output = "gh version"


class TestJQ(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "repo": "jqlang/jq",
            "binary": "jq",
            "raw": "jq",
            "raw_executable": True,
        }

    async def check_installed_command(self):
        return ["jq", "--version"]

    check_installed_output = "jq-"


class TestFiraCode(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"repo": "tonsky/FiraCode", "font": ["FiraCode-Regular"]}

    prerequisites = [{"system": "fontconfig"}]

    async def check_installed_command(self):
        return ["fc-list", "FiraCode"]

    check_installed_output = "FiraCode-Regular"

    affects_system = True

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("Overriding user's home doesn't work with fonts on macOS")


@pytest.mark.trio
async def test_skip_release():
    package = GitHubPackage(
        repo="atom/node-keytar",
        skip_release="v7.9.0",
        driver=LocalDriver(),
        db=DB.temporary(),
    )
    release = await package.release()
    assert release.tag_name == "v7.8.0"
