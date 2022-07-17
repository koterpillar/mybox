import json
import subprocess
from typing import Optional

from ..utils import run, run_output
from .pip_base import PipBasePackage

PIPX = ["python3", "-m", "pipx"]


def get_pipx_versions() -> dict[str, str]:
    pipx_list = json.loads(
        run_output(*PIPX, "list", "--json", stderr=subprocess.DEVNULL)
    )
    packages = (
        item["metadata"]["main_package"] for item in pipx_list["venvs"].values()
    )
    return {package["package"]: package["package_version"] for package in packages}


class PipxPackage(PipBasePackage):
    def __init__(self, *, pipx: str, **kwargs) -> None:
        self.package = pipx
        super().__init__(**kwargs)

    @property
    def local_version(self) -> Optional[str]:
        return get_pipx_versions().get(self.package)

    def install(self) -> None:
        cmd = "install" if self.local_version is None else "upgrade"
        run(*PIPX, cmd, self.package)
