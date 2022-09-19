import os
from typing import Any

from .base import PackageTestBase


class TestClone(PackageTestBase):
    @property
    def constructor_args(self) -> dict[str, Any]:
        return {
            "clone": "ohmyzsh/ohmyzsh",
            "destination": "oh-my-zsh",
        }

    @property
    def check_installed_command(self) -> list[str]:
        return [
            "cat",
            f"{os.environ['MYBOX_HOME']}/oh-my-zsh/templates/zshrc.zsh-template",
        ]

    check_installed_output = "alias ohmyzsh"
