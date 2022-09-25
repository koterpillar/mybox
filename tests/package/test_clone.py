from .base import DestinationPackageTestBase, PackageArgs, RootPackageTestBase


class TestClone(DestinationPackageTestBase, RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": self.dir_name,
            "root": self.root,
        }

    @property
    def check_installed_command(self) -> list[str]:
        return [
            "cat",
            str(self.destination / "templates/zshrc.zsh-template"),
        ]

    check_installed_output = "alias ohmyzsh"


class TestRootClone(TestClone):
    root = True

    root_required_for_is_installed = True
