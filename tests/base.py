import os
from pathlib import Path
from typing import Optional

from mybox.driver import Driver, RunResult, RunResultOutput
from mybox.utils import RunArg

CI: bool = "CI" in os.environ

DOCKER_IMAGE: Optional[str] = os.environ.get("DOCKER_IMAGE") or None

DOCKER: bool = DOCKER_IMAGE is not None


PACKAGE_ROOT: Path = Path(__file__).parent.parent.absolute()


class DummyDriver(Driver):
    commands: list[list[str]]

    def __init__(
        self,
        *,
        commands: Optional[list[list[str]]] = None,
        hostname: str = "mybox-host",
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.commands = commands if commands is not None else []
        self.hostname = hostname

    def reset(self) -> None:
        self.commands[:] = []

    async def run_(
        self,
        *args: RunArg,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        output = ""

        args_ = [str(arg) for arg in args]

        if args_[0] == "uname":
            output = "Linux"
        elif args_[0] == "hostname":
            output = self.hostname
        elif args_[0] == "cat":
            if args_[1] == "/etc/os-release":
                output = "ID=ubuntu"

        self.commands.append(args_)

        return RunResultOutput(ok=True, output=output)
