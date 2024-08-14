from pathlib import Path

import pytest

from mybox.driver import Driver, LocalDriver
from mybox.package.github import GitHubPackage
from mybox.state import DB

from .base import PackageArgs, PackageTestBase


class TestNeovim(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        args: PackageArgs = {
            "repo": "neovim/neovim",
            "binary": "nvim",
            "root": self.root,
        }
        if (await self.driver.os()).switch(linux=True, macos=False):
            args["app"] = "nvim"
        return args

    @property
    def check_driver(self) -> Driver:
        # Ordinary users should be able to use the package even if installed as root
        return super().check_driver.with_root(False)

    async def check_installed_command(self):
        return ["nvim", "--version"]

    check_installed_output = "NVIM"

    async def check_installed(self):
        command = await self.check_installed_command()
        version = await self.check_driver.run_output(*command)

        assert "NVIM" in version
        assert "-dev-" not in version

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
            "regex": r"^2\.13-3\.[0-9]+\.[0-9]+-M[0-9]-bootstrap$",
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

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {
            await self.check_driver.local() / "share" / "fonts" / ".uuid",
        }


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


@pytest.mark.trio
async def test_no_releases():
    package = GitHubPackage(
        repo="NixOS/nixpkgs", driver=LocalDriver(), db=DB.temporary()
    )

    with pytest.raises(ValueError, match="No releases found for NixOS/nixpkgs"):
        await package.release()
