import abc
from typing import Any

from mybox.package import parse_package


class PackageTestBase(metaclass=abc.ABCMeta):
    @property
    @abc.abstractmethod
    def constructor_args(self) -> dict[str, Any]:
        pass

    def test_installs(self):
        package = parse_package(self.constructor_args)
        package.ensure()
