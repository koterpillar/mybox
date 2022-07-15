import os
from typing import Optional

from ..utils import run, run_output, with_os
from .base import Package


def get_current_linux_shell() -> str:
    return run_output("getent", "passwd", os.environ["USER"]).split(":")[6].strip()


def get_current_macos_shell() -> str:
    return run_output("dscl", ".", "-read", os.environ["HOME"], "UserShell").split(
        ": "
    )[1]


SHELLS_FILE = "/etc/shells"


def get_all_shells() -> list[str]:
    with open(SHELLS_FILE) as shells_file:
        return [shell.strip() for shell in shells_file]


class Shell(Package):
    def __init__(self, shell: str, **kwargs) -> None:
        self.shell = shell
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return "_shell"

    def get_remote_version(self) -> str:
        return self.shell

    @property
    def local_version(self) -> Optional[str]:
        return with_os(linux=get_current_linux_shell, macos=get_current_macos_shell)()

    def install(self) -> None:
        if not os.path.exists(self.shell):
            raise ValueError(f"{self.shell} does not exist.")
        if not os.access(self.shell, os.X_OK):
            raise ValueError(f"{self.shell} is not executable.")
        if self.shell not in get_all_shells():
            run("sudo", "tee", "-a", SHELLS_FILE, input=self.shell.encode())
        run("chsh", "-s", self.shell)
