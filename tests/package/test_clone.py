import random
from functools import cached_property
from pathlib import Path
from typing import Any

from mybox.driver import Driver

from .base import PackageTestBase


class TestClone(PackageTestBase):
    @cached_property
    def dir_name(self) -> str:
        return f"mybox_test_{random.randint(0, 1000000)}"

    root = False

    @property
    def constructor_args(self) -> dict[str, Any]:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": self.dir_name,
            "root": self.root,
        }

    @property
    def test_driver(self) -> Driver:
        return super().test_driver.with_root(self.root)

    @property
    def target_dir(self) -> Path:
        return self.test_driver.home()

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

    def test_installs(self):
        try:
            return super().test_installs()
        finally:
            self.test_driver.run("rm", "-rf", str(self.destination))


class TestRootClone(TestClone):
    root = True
