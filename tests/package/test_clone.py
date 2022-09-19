import os
from pathlib import Path
from typing import Any

from mybox.utils import run

from .base import PackageTestBase


class TestClone(PackageTestBase):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": "oh-my-zsh",
        }

    @property
    def destination_dir(self) -> str:
        return os.environ["MYBOX_HOME"]

    @property
    def check_installed_command(self) -> list[str]:
        return [
            "cat",
            f"{self.destination_dir}/oh-my-zsh/templates/zshrc.zsh-template",
        ]

    check_installed_output = "alias ohmyzsh"


class TestRootClone(TestClone):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return super().constructor_args | {"root": True}

    def test_installs(self):
        try:
            return super().test_installs()
        finally:
            run("sudo", "rm", "-rf", f"{self.destination_dir}/oh-my-zsh")

    @property
    def destination_dir(self) -> str:
        return str(Path("~root").expanduser())

    @property
    def check_installed_command(self) -> list[str]:
        return ["sudo", *super().check_installed_command]
