import json
import subprocess
from typing import Optional

from ..utils import run, run_output
from .pip_base import PipBasePackage


def pip_cmd(cmd: str, /, *args: str) -> list[str]:
    return ["python3", "-m", "pip", cmd, "--user", *args]


def get_pip_versions() -> dict[str, str]:
    packages = json.loads(
        run_output(*pip_cmd("list", "--format", "json"), stderr=subprocess.DEVNULL)
    )
    return {package["name"]: package["version"] for package in packages}


class PipPackage(PipBasePackage):
    def __init__(self, *, pip: str, **kwargs) -> None:
        self.package = pip
        super().__init__(**kwargs)

    @property
    def local_version(self) -> Optional[str]:
        return get_pip_versions().get(self.package)

    def install(self) -> None:
        cmd = "install" if self.local_version is None else "upgrade"
        run(*pip_cmd(cmd, self.package))
