from .base import DestinationPackageTestBase, PackageArgs, RootPackageTestBase


class TestClone(DestinationPackageTestBase, RootPackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": self.dir_name,
            "root": self.root,
        }

    async def check_installed_command(self):
        return ["cat", await self.destination() / "templates/zshrc.zsh-template"]

    check_installed_output = "alias ohmyzsh"


class TestRootClone(TestClone):
    root = True

    root_required_for_is_installed = True


class TestURLClone(TestClone):
    async def constructor_args(self) -> PackageArgs:
        return await super().constructor_args() | {
            "clone": "https://github.com/ohmyzsh/ohmyzsh.git"
        }
