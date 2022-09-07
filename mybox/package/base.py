from abc import ABCMeta, abstractmethod
from functools import cached_property
from typing import Optional

from ..state import DB, VERSIONS, Table, Version
from ..utils import CURRENT_DISTRIBUTION, CURRENT_OS, OS, Distribution, Some, unsome_


class Package(metaclass=ABCMeta):
    os: Optional[list[OS]]
    distribution: Optional[list[Distribution]]

    def __init__(
        self,
        *,
        db: DB,
        os: Some[OS] = None,  # pylint:disable=redefined-outer-name
        distribution: Some[Distribution] = None,
    ) -> None:
        self.db = db
        self.os = unsome_(os)
        self.distribution = unsome_(distribution)

    @property
    @abstractmethod
    def name(self) -> str:
        pass

    @abstractmethod
    def get_remote_version(self) -> str:
        pass

    @cached_property
    def remote_version(self) -> str:
        return self.get_remote_version()

    @property
    @abstractmethod
    def local_version(self) -> Optional[str]:
        pass

    @property
    def is_installed(self) -> bool:
        return self.remote_version == self.local_version

    @abstractmethod
    def install(self) -> None:
        pass

    @property
    def applicable(self) -> bool:
        if self.os is not None and CURRENT_OS not in self.os:
            return False
        if (
            CURRENT_DISTRIBUTION is not None
            and self.distribution is not None
            and CURRENT_DISTRIBUTION not in self.distribution
        ):
            return False
        return True

    def ensure(self) -> bool:
        if not self.applicable:
            return False
        if self.is_installed:
            return False
        self.install()
        return True

    @cached_property
    def versions(self) -> Table[Version]:
        return VERSIONS(self.db)
