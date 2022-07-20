from abc import ABCMeta, abstractmethod
from functools import cached_property
from typing import Optional

from ..utils import *


class Package(metaclass=ABCMeta):
    os: Optional[list[OS]]
    distribution: Optional[list[Distribution]]

    def __init__(
        self,
        os: Some[OS] = None,  # pylint:disable=redefined-outer-name
        distribution: Some[Distribution] = None,
    ) -> None:
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

    def ensure(self) -> bool:
        if self.os is not None and CURRENT_OS not in self.os:
            return False
        if (
            CURRENT_DISTRIBUTION is not None
            and self.distribution is not None
            and CURRENT_DISTRIBUTION not in self.distribution
        ):
            return False
        if self.is_installed:
            return False
        self.install()
        return True
