import os
import pwd
from pathlib import Path
from typing import Optional

from ..fs import HOME, is_executable
from ..utils import run, run_output, with_os
from .base import Package


def get_current_linux_shell() -> str:
    return pwd.getpwuid(os.getuid()).pw_shell


def get_current_macos_shell() -> str:
    return run_output("dscl", ".", "-read", str(HOME), "UserShell").split(": ")[1]


SHELLS_FILE = "/etc/shells"


def get_all_shells() -> list[str]:
    with open(SHELLS_FILE) as shells_file:
        return [shell.strip() for shell in shells_file]


class Shell(Package):
    def __init__(self, shell: str, **kwargs) -> None:
        self.shell = Path(shell)
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return "_shell"

    def get_remote_version(self) -> str:
        return str(self.shell)

    @property
    def local_version(self) -> Optional[str]:
        return with_os(linux=get_current_linux_shell, macos=get_current_macos_shell)()

    def install(self) -> None:
        if not self.shell.is_file():
            raise ValueError(f"{self.shell} does not exist.")
        if not is_executable(self.shell):
            raise ValueError(f"{self.shell} is not executable.")
        if self.shell not in get_all_shells():
            run("sudo", "tee", "-a", SHELLS_FILE, input=str(self.shell).encode())
        run("chsh", "-s", str(self.shell))
