from mybox.package import Package

from .base import DestinationPackageTestBase, PackageArgs


class TestClone(DestinationPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": self.dir_name,
            "root": self.root,
        }

    async def check_installed_command(self):
        return ["cat", await self.destination() / "templates/zshrc.zsh-template"]

    check_installed_output = "alias ohmyzsh"


class TestCloneBranch(DestinationPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "clone": "node-fetch/node-fetch",
            "branch": "2.x",
            "destination": self.dir_name,
        }

    async def check_installed_command(self):
        return ["cat", await self.destination() / "package.json"]

    check_installed_output = '"version": "2'


class TestCloneBranchSwitch(TestClone):
    async def install_prerequisites(self, package: Package):
        await super().install_prerequisites(package)
        destination = await self.driver.home() / self.dir_name
        await self.driver.run(
            "git", "clone", "https://github.com/ohmyzsh/ohmyzsh.git", destination
        )
        await self.driver.run("git", "-C", destination, "checkout", "HEAD~")


class TestRootClone(TestClone):
    root = True

    root_required_for_is_installed = True


class TestURLClone(TestClone):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {
            "clone": "https://github.com/ohmyzsh/ohmyzsh.git"
        }
