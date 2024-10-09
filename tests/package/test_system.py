import pytest

from mybox.package import Package

from .base import PackageArgs, PackageTestBase


class TestRipGrep(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "ripgrep"}

    async def check_installed_command(self):
        return ["rg", "--help"]

    check_installed_output = "ripgrep"


class TestCask(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "alacritty"}

    async def check_installed_command(self):
        return ["alacritty", "--version"]

    check_installed_output = "alacritty 0"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda _: pytest.skip("Cask test is only applicable on macOS"),
            macos=lambda: None,
        )


class CaskFormulaPrecedence(PackageTestBase):
    affects_system = True

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda _: pytest.skip(
                "Cask precedence test is only applicable on macOS"
            ),
            macos=lambda: None,
        )

    async def constructor_args(self) -> PackageArgs:
        # angband exists as both:
        # https://formulae.brew.sh/formula/angband (formula)
        # https://formulae.brew.sh/cask/angband (cask)
        return {"system": "angband"}

    async def check_installed_command(self):
        # Formula installs to homebrew bin, cask installs to /Applications
        # Cask can't run due to missing signature, just check that it exists
        return ["ls", "/Applications/Angband.app/Contents/MacOS"]

    async def check_installed(self):
        await super().check_installed()
        # Formula-installed executable should not exist
        executable_exists = await self.driver.executable_exists("angband")
        assert not executable_exists, "Formula-installed executable exists"

    async def install_prerequisites(self, package: Package):
        await super().install_prerequisites(package)
        await self.driver.run_(
            "brew", "uninstall", "--cask", "angband", check=False, silent=True
        )
        await self.driver.run_("brew", "uninstall", "angband", check=False, silent=True)


class TestCaskFormulaPrecedenceNothingInstalled(CaskFormulaPrecedence):
    pass


class TestCaskFormulaPrecedenceFormulaInstalled(CaskFormulaPrecedence):
    async def install_prerequisites(self, package: Package):
        await super().install_prerequisites(package)
        await self.driver.run_ok("brew", "install", "angband")


class TestCaskFormulaPrecedenceCaskFormulaInstalled(CaskFormulaPrecedence):
    async def install_prerequisites(self, package: Package):
        await super().install_prerequisites(package)
        await self.driver.run_ok("brew", "install", "angband")
        await self.driver.run_ok("brew", "install", "--cask", "angband")


class TestRPMFusion(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {
            "system": "rpmfusion-free-release",
            "url": "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-40.noarch.rpm",
        }

    async def check_installed_command(self):
        return ["cat", "/etc/yum.repos.d/rpmfusion-free.repo"]

    check_installed_output = "RPM Fusion for Fedora"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution == "fedora"
                else pytest.skip("YUM repo test is only applicable on Fedora")
            ),
            macos=lambda: pytest.skip("YUM repo test is only applicable on Linux"),
        )


class TestInteractiveDeb(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "tzdata"}

    async def check_installed_command(self):
        return ["cat", "/usr/share/doc/tzdata/copyright"]

    check_installed_output = "Internet Assigned Numbers Authority"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution in {"debian", "ubuntu"}
                else pytest.skip("This test is only applicable on Debian and Ubuntu")
            ),
            macos=lambda: pytest.skip("This test is only applicable on Linux"),
        )


class TestVirtualPackage(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {"system": "g++"}

    async def check_installed_command(self):
        return ["g++", "--version"]

    check_installed_output = "Free Software Foundation, Inc."

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch(linux=True, macos=False):
            pytest.skip("This test is only applicable on Linux.")
