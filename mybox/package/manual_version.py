from abc import ABCMeta
from typing import Optional

from .base import Package, Version


class ManualVersion(Package, metaclass=ABCMeta):
    @property
    def local_version(self) -> Optional[str]:
        try:
            return self.versions[self.name].version
        except KeyError:
            return None

    def install(self) -> None:
        super().install()
        self.versions[self.name] = Version(version=self.get_remote_version())
