from typing import Iterable

from tests.package.driver import RootCheckDriver

from .base import DestinationPackageTestBase, PackageArgs, RootPackageTestBase


class TestClone(DestinationPackageTestBase, RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": self.dir_name,
            "root": self.root,
        }

    async def check_installed_command(self, driver: RootCheckDriver) -> Iterable[str]:
        return [
            "cat",
            str(await self.destination(driver) / "templates/zshrc.zsh-template"),
        ]

    check_installed_output = "alias ohmyzsh"


class TestRootClone(TestClone):
    root = True

    root_required_for_is_installed = True
