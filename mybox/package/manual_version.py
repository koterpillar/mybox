from abc import ABCMeta
from typing import Optional

from ..state import VERSIONS, Version
from .base import Package


class ManualVersion(Package, metaclass=ABCMeta):
    @property
    def local_version(self) -> Optional[str]:
        try:
            return VERSIONS[self.name].version
        except KeyError:
            return None

    def install(self) -> None:
        super().install()
        VERSIONS[self.name] = Version(version=self.get_remote_version())
