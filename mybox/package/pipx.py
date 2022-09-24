import json
from typing import Optional

from .pip_base import PipBasePackage

PIPX = ["python3", "-m", "pipx"]


class PipxPackage(PipBasePackage):
    def __init__(self, *, pipx: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.package = pipx

    def get_pipx_versions(self) -> dict[str, str]:
        pipx_list = json.loads(
            self.driver.run_output(*PIPX, "list", "--json", silent=True)
        )
        packages = (
            item["metadata"]["main_package"] for item in pipx_list["venvs"].values()
        )
        return {package["package"]: package["package_version"] for package in packages}

    @property
    def local_version(self) -> Optional[str]:
        return self.get_pipx_versions().get(self.package)

    def install(self) -> None:
        cmd = "install" if self.local_version is None else "upgrade"
        self.driver.run(*PIPX, cmd, self.package)
