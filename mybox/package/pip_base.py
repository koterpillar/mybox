from abc import ABCMeta

import requests

from .base import Package


class PipBasePackage(Package, metaclass=ABCMeta):
    package: str

    @property
    def name(self) -> str:
        return self.package

    def get_remote_version(self) -> str:
        pypi_info = requests.get(f"https://pypi.org/pypi/{self.package}/json").json()
        return pypi_info["info"]["version"]
