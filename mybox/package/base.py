from abc import ABCMeta, abstractmethod
from functools import cached_property
from typing import Optional

from ..driver import Driver
from ..state import DB, VERSIONS, Table, Version
from ..utils import Some, unsome_


class Package(metaclass=ABCMeta):
    os: Optional[list[str]]
    distribution: Optional[list[str]]

    def __init__(
        self,
        *,
        db: DB,
        driver: Driver,
        os: Some[str] = None,
        distribution: Some[str] = None,
    ) -> None:
        self.db = db
        self.driver_ = driver
        self.os = unsome_(os)
        self.distribution = unsome_(distribution)

    @property
    def driver(self) -> Driver:
        return self.driver_

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
        def check_os(name: str) -> bool:
            return self.os is None or name in self.os

        return self.driver.os.switch_(
            linux=lambda linux: check_os("linux")
            and (self.distribution is None or linux.distribution in self.distribution),
            macos=check_os("darwin"),
        )

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
