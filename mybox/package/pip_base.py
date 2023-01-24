import re
from abc import ABCMeta, abstractmethod
from typing import Optional, cast

import requests

from .base import Package

PIP = ["python3", "-m", "pip"]


class PipBasePackage(Package, metaclass=ABCMeta):
    package: str

    @property
    def name(self) -> str:
        return self.package

    @abstractmethod
    async def get_all_versions(self) -> dict[str, str]:
        pass

    async def local_version(self) -> Optional[str]:
        return (await self.get_all_versions()).get(self.package)

    async def _get_pypi_version(self) -> Optional[str]:
        pypi_info = requests.get(f"https://pypi.org/pypi/{self.package}/json").json()
        try:
            return pypi_info["info"]["version"]
        except KeyError:
            return None

    async def _get_index_version(self) -> Optional[str]:
        check = await self.driver.run_(
            *PIP,
            "index",
            "versions",
            self.package,
            check=False,
            silent=True,
            capture_output=True,
        )
        if not check.ok:
            return None
        output = cast(str, check.output)
        version = re.search(r"\(([^)]+)\)", output)
        if not version:
            raise Exception(f"Cannot parse pip output: {output}")
        return version[1]

    async def get_remote_version(self) -> str:
        if version := await self._get_pypi_version():
            return version

        if version := await self._get_index_version():
            return version

        raise Exception(f"Cannot find latest version of package '{self.package}'.")

    @abstractmethod
    def cmd(self, cmd: str, /, *args: str) -> list[str]:
        pass

    async def install(self) -> None:
        cmd = "install" if await self.local_version() is None else "upgrade"
        await self.driver.run(*self.cmd(cmd, self.package))
        await super().install()
