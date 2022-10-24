from abc import ABCMeta, abstractmethod
from typing import Optional

import requests

from .base import Package


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

    async def get_remote_version(self) -> str:
        pypi_info = requests.get(f"https://pypi.org/pypi/{self.package}/json").json()
        return pypi_info["info"]["version"]
