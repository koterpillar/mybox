from functools import cached_property
from pathlib import Path
from typing import Optional

from .root import Root

SHELLS_FILE = Path("/etc/shells")


class Shell(Root):
    def __init__(self, shell: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.shell = Path(shell)

    @property
    def name(self) -> str:
        return "_shell"

    def get_remote_version(self) -> str:
        return str(self.shell)

    def get_local_version_linux(self) -> str:
        return (
            self.driver.with_root(False)
            .run_output("getent", "passwd", self.driver.username)
            .split(":")[6]
        )

    def get_local_version_macos(self) -> str:
        return (
            self.driver.with_root(False)
            .run_output(
                "dscl", ".", "-read", f"/Users/{self.driver.username}", "UserShell"
            )
            .split(": ")[1]
        )

    @property
    def local_version(self) -> Optional[str]:
        return self.driver.os.switch(
            linux=self.get_local_version_linux,
            macos=self.get_local_version_macos,
        )()

    @cached_property
    def all_shells(self) -> list[str]:
        return self.driver.read_file(SHELLS_FILE).splitlines()

    def install(self) -> None:
        if not self.driver.is_file(self.shell):
            raise ValueError(f"{self.shell} does not exist.")
        if not self.driver.is_executable(self.shell):
            raise ValueError(f"{self.shell} is not executable.")
        if str(self.shell) not in self.all_shells:
            self.driver.with_root(True).run(
                "tee", "-a", str(SHELLS_FILE), input=str(self.shell).encode()
            )
        self.driver.run("chsh", "-s", str(self.shell))
