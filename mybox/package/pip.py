import json
from typing import Optional

from .pip_base import PipBasePackage


def pip_cmd(cmd: str, /, *args: str) -> list[str]:
    return ["python3", "-m", "pip", cmd, "--user", *args]


class PipPackage(PipBasePackage):
    def __init__(self, *, pip: str, **kwargs) -> None:
        self.package = pip
        super().__init__(**kwargs)

    def get_pip_versions(self) -> dict[str, str]:
        packages_json = self.driver.run_output(
            *pip_cmd("list", "--format", "json"), silent=True
        )
        packages = json.loads(packages_json)
        return {package["name"]: package["version"] for package in packages}

    @property
    def local_version(self) -> Optional[str]:
        return self.get_pip_versions().get(self.package)

    def install(self) -> None:
        cmd = "install" if self.local_version is None else "upgrade"
        self.driver.run(*pip_cmd(cmd, self.package))
