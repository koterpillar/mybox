import os
import random
from functools import cached_property
from pathlib import Path
from typing import Any

from mybox.utils import run

from .base import PackageTestBase


class TestClone(PackageTestBase):
    @cached_property
    def dir_name(self) -> str:
        return f"mybox_test_{random.randint(0, 1000000)}"

    @property
    def constructor_args(self) -> dict[str, Any]:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": self.dir_name,
        }

    @property
    def target_dir(self) -> Path:
        return Path(os.environ["MYBOX_HOME"])

    @property
    def destination(self) -> Path:
        return self.target_dir / self.dir_name

    @property
    def check_installed_command(self) -> list[str]:
        return [
            "cat",
            str(self.destination / "templates/zshrc.zsh-template"),
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
            run("rm", "-rf", str(self.destination), sudo=True)

    @property
    def target_dir(self) -> Path:
        return Path("~root").expanduser()

    @property
    def check_installed_command(self) -> list[str]:
        return ["sudo", *super().check_installed_command]
